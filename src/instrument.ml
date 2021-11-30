module DfSan = struct
  module NodeInfo = struct
    type t = Yojson.Safe.t

    let cmd_of t =
      ((t |> function
        | `Assoc l -> List.assoc "cmd" l
        | _ -> failwith "Invalid format")
       |> function
       | `List l -> List.hd l
       | _ -> failwith "Invalid format")
      |> function
      | `String s -> s
      | _ -> failwith "Invalid format"

    let filename_of t =
      t
      |> (function
           | `Assoc l -> List.assoc "loc" l | _ -> failwith "Invalid format")
      |> (function `String s -> s | _ -> failwith "Invalid format")
      |> String.split_on_char ':' |> Fun.flip List.nth 0
  end

  module NodeInfoMap = struct
    module M = Map.Make (String)

    type t = NodeInfo.t M.t

    let empty = M.empty

    let add = M.add

    let find = M.find
  end

  module LineSet = Set.Make (String)
  module FileToEdges = Map.Make (String)

  let read_nodes file =
    let ic = open_in file in
    Yojson.Safe.from_channel ic
    |> (function
         | `Assoc l -> List.assoc "nodes" l | _ -> failwith "Invalid format")
    |> (function `Assoc l -> l | _ -> failwith "Invalid format")
    |> List.fold_left
         (fun map (name, info) -> NodeInfoMap.add name info map)
         NodeInfoMap.empty
    |> fun x ->
    close_in ic;
    x

  let read_covered_lines file =
    let ic = open_in file in
    let rec loop lst =
      match input_line ic with
      | line -> (
          String.split_on_char '\t' line |> function
          | h :: _ -> loop (LineSet.add h lst)
          | _ -> failwith "Invalid format")
      | exception End_of_file -> lst
    in
    loop LineSet.empty |> fun x ->
    close_in ic;
    x

  let read_duedges nodes file =
    let ic = open_in file in
    let rec loop map =
      match input_line ic with
      | line -> (
          String.split_on_char '\t' line |> function
          | src :: dst :: _ ->
              let file = NodeInfoMap.find src nodes |> NodeInfo.filename_of in
              FileToEdges.update file
                (function
                  | None -> Some [ (src, dst) ]
                  | Some l -> Some ((src, dst) :: l))
                map
              |> loop
          | _ -> failwith "Invalid format")
      | exception End_of_file -> map
    in
    loop FileToEdges.empty |> fun x ->
    close_in ic;
    x

  type dfsan_funs = {
    create_label : Cil.varinfo;
    set_label : Cil.varinfo;
    get_label : Cil.varinfo;
    has_label : Cil.varinfo;
  }

  let initialize work_dir =
    let result_file = Filename.concat work_dir "localizer-out/result.txt" in
    let sparrow_out_dir = Filename.concat work_dir "sparrow-out" in
    let node_file = Filename.concat sparrow_out_dir "node.json" in
    let duedge_file =
      Filename.concat sparrow_out_dir "interval/datalog/DUEdge.facts"
    in
    let nodes = read_nodes node_file in
    let lines = read_covered_lines result_file in
    let duedges = read_duedges nodes duedge_file in
    (nodes, lines, duedges)

  let rec instrument_instr dfsan_funs edges instrs results =
    match instrs with
    | (Cil.Set ((Var vi, NoOffset), _, loc) as i) :: tl ->
        let name = Cil.mkString vi.vname in
        Cil.Call
          ( None,
            Cil.Lval (Cil.Var dfsan_funs.create_label, Cil.NoOffset),
            [ name; Cil.zero ],
            loc )
        :: i :: results
        |> instrument_instr dfsan_funs edges tl
    | i :: tl -> i :: results |> instrument_instr dfsan_funs edges tl
    | [] -> List.rev results

  class assignVisitor dfsan_funs edges =
    object
      inherit Cil.nopCilVisitor

      method! vstmt s =
        match s.Cil.skind with
        | Cil.Instr i ->
            s.Cil.skind <- Cil.Instr (instrument_instr dfsan_funs edges i []);
            DoChildren
        | _ -> DoChildren
    end

  let instrument file pp_file _ edges =
    Logging.log "Instrument %s (%s)" file pp_file;
    let cil = Frontc.parse pp_file () in
    let dfsan_funs =
      {
        create_label =
          Cil.findOrCreateFunc cil "dfsan_create_label"
            (Cil.TFun (Cil.voidType, None, false, []));
        set_label =
          Cil.findOrCreateFunc cil "dfsan_set_label"
            (Cil.TFun (Cil.voidType, None, false, []));
        get_label =
          Cil.findOrCreateFunc cil "dfsan_get_label"
            (Cil.TFun (Cil.voidType, None, false, []));
        has_label =
          Cil.findOrCreateFunc cil "dfsan_has_label"
            (Cil.TFun (Cil.voidType, None, false, []));
      }
    in
    Cil.visitCilFile (new assignVisitor dfsan_funs edges) cil;
    let oc = open_out pp_file in
    Cil.dumpFile !Cil.printerForMaincil oc "" cil;
    close_out oc

  let run work_dir src_dir =
    let nodes, _, duedges = initialize work_dir in
    FileToEdges.iter
      (fun file edges ->
        if file = "" then ()
        else
          let name = Filename.remove_extension file in
          let pp_file = Filename.concat src_dir (name ^ ".i") in
          if Sys.file_exists pp_file then instrument file pp_file nodes edges
          else Logging.log "%s not found" file)
      duedges
end

let preamble src_dir mode =
  String.concat ""
    [
      "/* COVERAGE :: INSTRUMENTATION :: START */\n";
      "typedef struct _IO_FILE FILE;";
      "struct _IO_FILE *__inst_stream ;";
      "extern FILE *fopen(char const   * __restrict  __filename , char const   \
       * __restrict  __modes ) ;";
      "extern int fclose(FILE *__stream ) ;";
      "static void coverage_ctor (void) __attribute__ ((constructor));\n";
      "static void coverage_ctor (void) {\n";
      "  __inst_stream = fopen(\"" ^ src_dir ^ "/" ^ mode ^ ".txt"
      ^ "\", \"a\");\n";
      "  fprintf(__inst_stream, \"__START_NEW_EXECUTION__\\n\");\n";
      "  fflush(__inst_stream);\n";
      "}\n";
      "static void coverage_dtor (void) __attribute__ ((destructor));\n";
      "static void coverage_dtor (void) {\n";
      "  fclose(__inst_stream);\n";
      "}\n";
      "/* COVERAGE :: INSTRUMENTATION :: END */\n";
    ]

let found_type = ref None

let found_gvar = ref None

class findTypeVisitor name =
  object
    inherit Cil.nopCilVisitor

    method! vglob g =
      match g with
      | GCompTag (ci, _) ->
          if ci.Cil.cname = name then found_type := Some ci;
          SkipChildren
      | _ -> SkipChildren
  end

class findGVarVisitor name =
  object
    inherit Cil.nopCilVisitor

    method! vglob g =
      match g with
      | GVarDecl (vi, _) ->
          if vi.Cil.vname = name then found_gvar := Some vi;
          SkipChildren
      | _ -> SkipChildren
  end

let append_constructor work_dir filename mode =
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let instr_c_code = preamble work_dir mode ^ read_whole_file filename in
  let oc = open_out filename in
  Printf.fprintf oc "%s" instr_c_code;
  close_out oc

module GSA = struct
  let pred_num = ref (-1)

  let new_pred () =
    pred_num := !pred_num + 1;
    "OOJAHOOO_PRED_" ^ string_of_int !pred_num

  class assignInitializer f =
    let add_predicate_var result stmt =
      match stmt.Cil.skind with
      | Cil.If (pred, then_branch, else_branch, loc) ->
          let pred_var = new_pred () in
          let vi = Cil.makeLocalVar f pred_var (Cil.TInt (Cil.IInt, [])) in
          stmt.Cil.skind <-
            Cil.If
              ( Cil.Lval (Cil.Var vi, Cil.NoOffset),
                then_branch,
                else_branch,
                loc );
          result
          @ [
              Cil.mkStmtOneInstr
                (Cil.Set ((Cil.Var vi, Cil.NoOffset), pred, loc));
              stmt;
            ]
      | _ -> result @ [ stmt ]
    in
    object
      inherit Cil.nopCilVisitor

      method! vblock b =
        let new_stmts = List.fold_left add_predicate_var [] b.Cil.bstmts in
        b.bstmts <- new_stmts;
        DoChildren
    end

  class predicateVisitor =
    object
      inherit Cil.nopCilVisitor

      method! vfunc f =
        if
          String.length f.svar.vname >= 6
          && (String.equal (String.sub f.svar.vname 0 6) "bugzoo"
             || String.equal (String.sub f.svar.vname 0 6) "unival")
        then SkipChildren
        else ChangeTo (Cil.visitCilFunction (new assignInitializer f) f)
    end

  let predicate_transform pp_file =
    let origin_file = Filename.basename (Filename.basename pp_file) in
    Logging.log "Predicate transform %s (%s)" origin_file pp_file;
    let cil_opt =
      try Some (Frontc.parse pp_file ()) with Frontc.ParseError _ -> None
    in
    if Option.is_none cil_opt then pp_file
    else
      let cil = Option.get cil_opt in
      Cil.visitCilFile (new predicateVisitor) cil;
      let oc = open_out pp_file in
      Cil.dumpFile !Cil.printerForMaincil oc "" cil;
      close_out oc;
      pp_file

  module CausalMap = Map.Make (String)
  module VarSet = Set.Make (String)
  module VarVerMap = Map.Make (String)
  module VarMap = Map.Make (String)

  let causal_map = ref CausalMap.empty

  let var_ver = ref VarVerMap.empty

  class assignVisitor (printf, flush, stream) f =
    let vname_of lv =
      match lv with Cil.Var vi, Cil.NoOffset -> vi.Cil.vname | _ -> ""
    in
    let varinfo_of lv =
      match lv with
      | Cil.Var vi, Cil.NoOffset -> vi
      | _ -> Cil.makeVarinfo false "" (Cil.TVoid [])
    in
    let rec var_names_of exp =
      let result =
        match exp with
        | Cil.Lval lv -> VarMap.singleton (vname_of lv) (varinfo_of lv)
        | Cil.SizeOfE e -> var_names_of e
        | Cil.AlignOfE e -> var_names_of e
        | Cil.UnOp (_, e, _) -> var_names_of e
        | Cil.BinOp (_, e1, e2, _) ->
            VarMap.union
              (fun _ va1 _ -> Some va1)
              (var_names_of e1) (var_names_of e2)
        | Cil.Question (e1, e2, e3, _) ->
            VarMap.union
              (fun _ va1 _ -> Some va1)
              (VarMap.union
                 (fun _ va1 _ -> Some va1)
                 (var_names_of e1) (var_names_of e2))
              (var_names_of e3)
        | Cil.CastE (_, e) -> var_names_of e
        | _ -> VarMap.empty
      in
      VarMap.remove "" result
    in
    let is_pred vname =
      let pred_prefix = Str.regexp "OOJAHOOO_PRED_\[0-9\]\+" in
      Str.string_match pred_prefix vname 0
    in
    let rec string_of_typ = function
      | Cil.TInt (Cil.IChar, _) -> "char"
      | Cil.TInt (Cil.ISChar, _) -> "signed char"
      | Cil.TInt (Cil.IUChar, _) -> "unsigned char"
      | Cil.TInt (Cil.IInt, _) -> "int"
      | Cil.TInt (Cil.IUInt, _) -> "unsigned int"
      | Cil.TInt (Cil.IShort, _) -> "short"
      | Cil.TInt (Cil.IUShort, _) -> "unsigned short"
      | Cil.TInt (Cil.ILong, _) -> "long"
      | Cil.TInt (Cil.IULong, _) -> "unsigned long"
      | Cil.TFloat (Cil.FFloat, _) -> "float"
      | Cil.TFloat (Cil.FDouble, _) -> "double"
      | Cil.TFloat (Cil.FLongDouble, _) -> "long double"
      | Cil.TPtr (Cil.TInt (Cil.IChar, _), _) -> "string"
      | Cil.TNamed (t, _) -> string_of_typ t.ttype
      | _ -> "NA"
    in
    let call_record var vname ver loc =
      let call_printf filename funcname line varname version typ var_exp =
        let fmt =
          match typ with
          | "char" | "signed char" | "unsigned char" -> "%c"
          | "unsigned int" -> "%u"
          | "int" | "short" -> "%d"
          | "unsigned short" -> "%hd"
          | "long" -> "%ld"
          | "unsigned long" -> "%lu"
          | "float" -> "%f"
          | "double" | "long double" -> "%lf"
          | "string" -> "%s"
          | "NA" -> "NA"
          | _ -> ""
        in
        Cil.Call
          ( None,
            Cil.Lval (Cil.Var printf, Cil.NoOffset),
            [
              Cil.Lval (Cil.Var stream, Cil.NoOffset);
              Cil.Const
                (Cil.CStr
                   (Printf.sprintf "%s,%s,%d,%s,%d" filename funcname line
                      varname version
                   ^ "," ^ fmt ^ "\n"));
              var_exp;
            ],
            loc )
      in
      let call_flush loc =
        Cil.Call
          ( None,
            Cil.Lval (Cil.Var flush, Cil.NoOffset),
            [ Cil.Lval (Cil.Var stream, Cil.NoOffset) ],
            loc )
      in
      let fun_name = f.Cil.svar.vname in
      let t = string_of_typ (Cil.typeOfLval var) in
      if
        String.length vname >= 13
        && String.equal (String.sub vname 0 13) "OOJAHOOO_PRED"
      then
        [
          call_printf loc.Cil.file fun_name loc.Cil.line vname ver t
            (Cil.Question
               ( Cil.BinOp (Eq, Cil.Lval var, Cil.zero, Cil.intType),
                 Cil.zero,
                 Cil.one,
                 Cil.intType ));
          call_flush loc;
        ]
        (* printf("%s,%s,%d,%s,%d,%d\n", filename, funcname, line, varname, version, i_val) *)
      else
        [
          call_printf loc.Cil.file fun_name loc.Cil.line vname ver t (Lval var);
          call_flush loc;
        ]
    in
    let ass2gsa result instr =
      let gogo, lv, lval, exp_vars, loc =
        match instr with
        | Cil.Set (lv, exp, loc) ->
            let exp_vars = var_names_of exp in
            let lval = vname_of lv in
            (true, lv, lval, exp_vars, loc)
        | Call (lv_opt, _, params, loc) ->
            if Option.is_none lv_opt then
              ( false,
                (Var (Cil.makeVarinfo false "" (Cil.TVoid [])), Cil.NoOffset),
                "",
                VarMap.empty,
                loc )
            else
              let lv = Option.get lv_opt in
              let exp_vars =
                List.fold_left
                  (fun ev param ->
                    VarMap.union
                      (fun _ vi1 _ -> Some vi1)
                      ev (var_names_of param))
                  VarMap.empty params
              in
              let lval = vname_of lv in
              (true, lv, lval, exp_vars, loc)
        | _ ->
            ( false,
              (Var (Cil.makeVarinfo false "" (Cil.TVoid [])), Cil.NoOffset),
              "",
              VarMap.empty,
              { line = -1; file = ""; byte = -1 } )
      in
      if (not gogo) || lval = "" then result @ [ instr ]
      else if is_pred lval then (
        let exp_vars_with_ver, exp_vars_with_new_ver =
          VarMap.fold
            (fun ev _ (vs, nvs) ->
              (* for debugging *)
              (* print_endline "a"; *)
              let fn_ev = f.Cil.svar.vname ^ "_" ^ ev in
              if VarVerMap.mem fn_ev !var_ver then
                let ver = VarVerMap.find fn_ev !var_ver in
                ( (fn_ev ^ "_" ^ string_of_int ver) :: vs,
                  (fn_ev ^ "_" ^ string_of_int (ver + 1)) :: nvs )
              else if VarVerMap.mem ev !var_ver then
                let ver = VarVerMap.find ev !var_ver in
                ( (ev ^ "_" ^ string_of_int ver) :: vs,
                  (ev ^ "_" ^ string_of_int (ver + 1)) :: nvs )
              else raise (Failure "Not_Found_Var"))
            exp_vars ([], [])
        in
        let fn_lval = f.Cil.svar.vname ^ "_" ^ lval in
        causal_map := CausalMap.add fn_lval exp_vars_with_ver !causal_map;
        let fn_exp_vars =
          VarMap.fold
            (fun v _ fevs -> VarSet.add (f.Cil.svar.vname ^ "_" ^ v) fevs)
            exp_vars VarSet.empty
        in
        let new_var_ver =
          VarVerMap.mapi
            (fun v ver ->
              if VarSet.mem v fn_exp_vars || VarMap.mem v exp_vars then ver + 1
              else ver)
            !var_ver
        in
        List.iter2
          (fun old_ver new_ver ->
            causal_map := CausalMap.add new_ver [ old_ver ] !causal_map)
          exp_vars_with_ver exp_vars_with_new_ver;
        let pred_record = call_record lv lval 0 loc in
        let records =
          VarMap.fold
            (fun vname vi rs ->
              (* for debugging *)
              (* print_endline "b"; *)
              let fn_vname = f.Cil.svar.vname ^ "_" ^ vname in
              call_record (Cil.Var vi, Cil.NoOffset) vname
                (if VarMap.mem fn_vname new_var_ver then
                 VarMap.find fn_vname new_var_ver
                else VarMap.find vname new_var_ver)
                loc
              @ rs)
            exp_vars []
        in
        var_ver := new_var_ver;
        result @ instr :: (pred_record @ records))
      else
        let fn_lval = f.Cil.svar.vname ^ "_" ^ lval in
        let new_var_ver =
          if VarVerMap.mem fn_lval !var_ver then
            VarVerMap.update fn_lval
              (fun ver -> Some (Option.get ver + 1))
              !var_ver
          else if VarVerMap.mem lval !var_ver then
            VarVerMap.update lval
              (fun ver -> Some (Option.get ver + 1))
              !var_ver
          else VarVerMap.add fn_lval 0 !var_ver
        in
        let exp_vars_with_ver =
          VarMap.fold
            (fun ev _ vs ->
              (* for debugging *)
              (* print_endline ev; *)
              let fn_ev = f.Cil.svar.vname ^ "_" ^ ev in
              if VarVerMap.mem fn_ev !var_ver then
                let ver = VarVerMap.find fn_ev !var_ver in
                (fn_ev ^ "_" ^ string_of_int ver) :: vs
              else if VarVerMap.mem ev !var_ver then
                let ver = VarVerMap.find ev !var_ver in
                (ev ^ "_" ^ string_of_int ver) :: vs
              else raise (Failure "Not_Found_Var"))
            exp_vars []
        in
        (* for debugging *)
        (* print_endline "d"; *)
        let final_lval, ver_of_lval =
          if VarVerMap.mem fn_lval new_var_ver then
            (fn_lval, VarVerMap.find fn_lval new_var_ver)
          else (lval, VarVerMap.find lval new_var_ver)
        in
        let lval_with_ver = final_lval ^ "_" ^ string_of_int ver_of_lval in
        causal_map := CausalMap.add lval_with_ver exp_vars_with_ver !causal_map;
        let lv_record = call_record lv lval ver_of_lval loc in
        var_ver := new_var_ver;
        result @ instr :: lv_record
    in
    object
      inherit Cil.nopCilVisitor

      method! vstmt s =
        match s.Cil.skind with
        | Instr is ->
            s.Cil.skind <- Instr (List.fold_left ass2gsa [] is);
            DoChildren
        | _ -> DoChildren
    end

  class funAssignVisitor (printf, flush, stream) =
    object
      inherit Cil.nopCilVisitor

      method! vfunc f =
        if
          String.length f.svar.vname >= 6
          && String.equal (String.sub f.svar.vname 0 6) "unival"
        then Cil.SkipChildren
        else (
          List.iter
            (fun form ->
              var_ver :=
                VarVerMap.add
                  (f.Cil.svar.vname ^ "_" ^ form.Cil.vname)
                  0 !var_ver)
            f.Cil.sformals;
          List.iter
            (fun local ->
              var_ver :=
                VarVerMap.add
                  (f.Cil.svar.vname ^ "_" ^ local.Cil.vname)
                  0 !var_ver)
            f.Cil.slocals;
          ChangeTo
            (Cil.visitCilFunction
               (new assignVisitor (printf, flush, stream) f)
               f))
    end

  let extract_gvar globals =
    List.filter_map
      (fun g ->
        match g with
        | Cil.GVarDecl (vi, _) | Cil.GVar (vi, _, _) -> Some vi.Cil.vname
        | _ -> None)
      globals

  let gsa_gen work_dir pt_file =
    let origin_file_paths =
      Utils.find_file (Filename.remove_extension pt_file ^ ".c") work_dir
    in
    let ori_file_num = List.length origin_file_paths in
    if ori_file_num = 0 then ()
    else
      let origin_file = List.hd origin_file_paths in
      Logging.log "GSA_Gen %s (%s)" origin_file pt_file;
      let cil_opt =
        try Some (Frontc.parse pt_file ()) with Frontc.ParseError _ -> None
      in
      if Option.is_none cil_opt then ()
      else
        let cil = Option.get cil_opt in
        Cil.visitCilFile (new findTypeVisitor "_IO_FILE") cil;
        Cil.visitCilFile (new findGVarVisitor "stderr") cil;
        if Option.is_none !found_type || Option.is_none !found_gvar then ()
        else
          let fileptr = Cil.TPtr (Cil.TComp (Option.get !found_type, []), []) in
          let printf =
            Cil.findOrCreateFunc cil "fprintf"
              (Cil.TFun
                 ( Cil.voidType,
                   Some
                     [
                       ("stream", fileptr, []); ("format", Cil.charPtrType, []);
                     ],
                   true,
                   [] ))
          in
          let flush =
            Cil.findOrCreateFunc cil "fflush"
              (Cil.TFun
                 (Cil.voidType, Some [ ("stream", fileptr, []) ], false, []))
          in
          let stream = Cil.makeGlobalVar "__inst_stream" fileptr in
          cil.Cil.globals <-
            Cil.GVarDecl (stream, Cil.locUnknown) :: cil.globals;
          let global_vars = extract_gvar cil.Cil.globals in
          var_ver :=
            List.fold_left
              (fun vv gv -> VarVerMap.add gv 0 vv)
              VarVerMap.empty global_vars;
          Cil.visitCilFile (new funAssignVisitor (printf, flush, stream)) cil;
          Unix.system
            ("cp " ^ origin_file ^ " "
            ^ Filename.remove_extension origin_file
            ^ ".origin.c")
          |> ignore;
          (if
           List.mem (Filename.basename origin_file) [ "proc_open.c"; "cast.c" ]
          then ()
          else
            let oc = open_out origin_file in
            Cil.dumpFile !Cil.printerForMaincil oc "" cil;
            close_out oc);
          if
            List.mem
              (Filename.basename origin_file)
              [ "gzip.c"; "tif_unix.c"; "http_auth.c"; "main.c" ]
          then append_constructor work_dir origin_file "unival"

  let print_cm work_dir causal_map =
    let output_file = Filename.concat work_dir "CausalMap.txt" in
    let oc = open_out output_file in
    let cm_str =
      Utils.join
        (CausalMap.fold
           (fun var parents res -> Utils.join (var :: parents) "," :: res)
           causal_map [])
        "\n"
    in
    Printf.fprintf oc "%s" cm_str;
    close_out oc

  let print_fc work_dir causal_map =
    let output_file = Filename.concat work_dir "FaultCandidates.txt" in
    let oc = open_out output_file in
    let fc_str =
      Utils.join
        (CausalMap.fold
           (fun var _ res ->
             let var_without_ver =
               Utils.join
                 (List.rev (List.tl (List.rev (String.split_on_char '_' var))))
                 "_"
             in
             if List.mem var_without_ver res then res
             else var_without_ver :: res)
           causal_map [])
        "_1\n"
    in
    Printf.fprintf oc "%s" fc_str;
    close_out oc

  let run work_dir src_dir =
    Utils.traverse_pp_file
      (fun pp_file -> pp_file |> predicate_transform |> gsa_gen work_dir)
      src_dir;
    Utils.remove_temp_files src_dir;
    print_cm work_dir !causal_map;
    print_fc work_dir !causal_map
end

module Coverage = struct
  let location_of_instr = function
    | Cil.Set (_, _, l) | Cil.Call (_, _, _, l) | Cil.Asm (_, _, _, _, _, l) ->
        l

  let printf_of printf stream loc =
    Cil.Call
      ( None,
        Cil.Lval (Cil.Var printf, Cil.NoOffset),
        [
          Cil.Lval (Cil.Var stream, Cil.NoOffset);
          Cil.Const (Cil.CStr "%s:%d\n");
          Cil.Const (Cil.CStr loc.Cil.file);
          Cil.integer loc.Cil.line;
        ],
        loc )

  let flush_of flush stream loc =
    Cil.Call
      ( None,
        Cil.Lval (Cil.Var flush, Cil.NoOffset),
        [ Cil.Lval (Cil.Var stream, Cil.NoOffset) ],
        loc )

  class instrumentVisitor printf flush stream =
    object
      inherit Cil.nopCilVisitor

      method! vfunc fd =
        if fd.Cil.svar.vname = "bugzoo_ctor" then SkipChildren else DoChildren

      method! vblock blk =
        let bstmts =
          List.fold_left
            (fun bstmts s ->
              match s.Cil.skind with
              | Cil.Instr insts ->
                  let new_insts =
                    List.fold_left
                      (fun is i ->
                        let loc = Cil.get_instrLoc i in
                        let call = printf_of printf stream loc in
                        let flush = flush_of flush stream loc in
                        i :: flush :: call :: is)
                      [] insts
                    |> List.rev
                  in
                  s.skind <- Cil.Instr new_insts;
                  s :: bstmts
              | _ ->
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  let call =
                    printf_of printf stream loc |> Cil.mkStmtOneInstr
                  in
                  let flush = flush_of flush stream loc |> Cil.mkStmtOneInstr in
                  s :: flush :: call :: bstmts)
            [] blk.Cil.bstmts
          |> List.rev
        in
        blk.bstmts <- bstmts;
        Cil.DoChildren
    end

  let instrument work_dir pt_file =
    Cil.resetCIL ();
    let origin_file = Filename.remove_extension pt_file ^ ".c" in
    Logging.log "Instrument Coverage %s (%s)" origin_file pt_file;
    let cil_opt =
      try Some (Frontc.parse pt_file ()) with
      | Frontc.ParseError _ -> None
      | Stack_overflow ->
          Logging.log "%s" "Stack overflow";
          None
      | e ->
          Logging.log "%s" (Printexc.to_string e);
          None
    in
    if Option.is_none cil_opt then ()
    else
      let cil = Option.get cil_opt in
      (* TODO: clean up *)
      Cil.visitCilFile (new findTypeVisitor "_IO_FILE") cil;
      Cil.visitCilFile (new findGVarVisitor "stderr") cil;
      if Option.is_none !found_type || Option.is_none !found_gvar then ()
      else
        let fileptr = Cil.TPtr (Cil.TComp (Option.get !found_type, []), []) in
        let printf =
          Cil.findOrCreateFunc cil "fprintf"
            (Cil.TFun
               ( Cil.voidType,
                 Some
                   [ ("stream", fileptr, []); ("format", Cil.charPtrType, []) ],
                 true,
                 [] ))
        in
        let flush =
          Cil.findOrCreateFunc cil "fflush"
            (Cil.TFun (Cil.voidType, Some [ ("stream", fileptr, []) ], false, []))
        in
        let stream = Cil.makeGlobalVar "__inst_stream" fileptr in
        cil.globals <- Cil.GVarDecl (stream, Cil.locUnknown) :: cil.globals;
        Cil.visitCilFile (new instrumentVisitor printf flush stream) cil;
        Unix.system
          ("cp " ^ origin_file ^ " "
          ^ Filename.remove_extension origin_file
          ^ ".origin.c")
        |> ignore;
        (if List.mem (Filename.basename origin_file) [ "proc_open.c"; "cast.c" ]
        then ()
        else
          let oc = open_out origin_file in
          Cil.dumpFile !Cil.printerForMaincil oc "" cil;
          close_out oc);
        if
          List.mem
            (Filename.basename origin_file)
            [ "gzip.c"; "tif_unix.c"; "http_auth.c"; "main.c"; "version.c" ]
        then append_constructor work_dir origin_file "coverage"

  let run work_dir src_dir =
    Utils.traverse_pp_file (instrument work_dir) src_dir
end

let run work_dir =
  Cil.initCIL ();
  let src_dir = Filename.concat work_dir "src" in
  match !Cmdline.instrument with
  | Cmdline.DfSan -> DfSan.run work_dir src_dir
  | Cmdline.GSA -> GSA.run work_dir src_dir
  | Cmdline.Coverage -> Coverage.run work_dir src_dir
  | Cmdline.Nothing -> ()
