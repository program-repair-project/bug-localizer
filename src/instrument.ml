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
    ([
       "/* COVERAGE :: INSTRUMENTATION :: START */\n";
       "typedef struct _IO_FILE FILE;\n";
       "struct _IO_FILE *__inst_stream ;\n";
       "extern FILE *fopen(char const   * __restrict  __filename , char \
        const   * __restrict  __modes ) ;\n";
       "extern int fclose(FILE *__stream ) ;\n";
       "static void coverage_ctor (void) __attribute__ ((constructor));\n";
       "static void coverage_ctor (void) {\n";
     ]
    @ (if mode = "output" then
       [ "__inst_stream = fopen(\"" ^ src_dir ^ "/output.txt\", \"a\");\n" ]
      else
        [
          "  int pid = getpid();\n";
          "  char filename[64];\n";
          "  sprintf(filename, \"" ^ src_dir ^ "/coverage_data" ^ "/tmp/" ^ mode
          ^ "-%d.txt\", pid);\n";
          "  __inst_stream = fopen(filename, \"a\");\n";
          "  fprintf(__inst_stream, \"__START_NEW_EXECUTION__\\n\");\n";
          "  fflush(__inst_stream);\n";
        ])
    @ [
        "}\n";
        "static void coverage_dtor (void) __attribute__ ((destructor));\n";
        "static void coverage_dtor (void) {\n";
        "  fclose(__inst_stream);\n";
        "}\n";
        "/* COVERAGE :: INSTRUMENTATION :: END */\n";
      ])

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
  let code = read_whole_file filename in
  if
    String.length code > 42
    && String.equal (String.sub code 0 42)
         "/* COVERAGE :: INSTRUMENTATION :: START */"
  then ()
  else
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
          let assign =
            Cil.mkStmtOneInstr (Cil.Set ((Cil.Var vi, Cil.NoOffset), pred, loc))
          in
          let temp = assign.skind in
          assign.skind <- stmt.skind;
          stmt.skind <- temp;
          result @ [ stmt; assign ]
      | _ -> result @ [ stmt ]
    in
    object
      inherit Cil.nopCilVisitor

      method! vblock b =
        let new_stmts = List.fold_left add_predicate_var [] b.Cil.bstmts in
        b.bstmts <- new_stmts;
        DoChildren
    end

  class predicateVisitor faulty_func_list =
    object
      inherit Cil.nopCilVisitor

      method! vfunc f =
        if
          String.length f.svar.vname >= 6
          && (String.equal (String.sub f.svar.vname 0 6) "bugzoo"
             || String.equal (String.sub f.svar.vname 0 6) "unival")
          || List.length faulty_func_list > 0
             && not (List.mem f.svar.vname faulty_func_list)
        then SkipChildren
        else ChangeTo (Cil.visitCilFunction (new assignInitializer f) f)
    end

  let predicate_transform ?(faulty_func_list = []) pp_file =
    let origin_file = Filename.basename (Filename.basename pp_file) in
    Logging.log "Predicate transform %s (%s)" origin_file pp_file;
    let cil_opt =
      try Some (Frontc.parse pp_file ()) with Frontc.ParseError _ -> None
    in
    if Option.is_none cil_opt then pp_file
    else
      let cil = Option.get cil_opt in
      Cil.visitCilFile (new predicateVisitor faulty_func_list) cil;
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
      let pred_prefix = Str.regexp "OOJAHOOO_PRED_\\[0-9\\]\\+" in
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
                      ("UNIVAL_" ^ funcname ^ "_" ^ varname)
                      version
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
              let unival_fn_ev = "UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ ev in
              let unival_ev = "UNIVAL_" ^ ev in
              if VarVerMap.mem unival_fn_ev !var_ver then
                let ver = VarVerMap.find unival_fn_ev !var_ver in
                ( (unival_fn_ev ^ "_" ^ string_of_int ver) :: vs,
                  (unival_fn_ev ^ "_" ^ string_of_int (ver + 1)) :: nvs )
              else if VarVerMap.mem unival_ev !var_ver then
                let ver = VarVerMap.find ev !var_ver in
                ( (unival_ev ^ "_" ^ string_of_int ver) :: vs,
                  (unival_ev ^ "_" ^ string_of_int (ver + 1)) :: nvs )
              else (
                print_endline unival_fn_ev;
                raise (Failure "Not_Found_Var")))
            exp_vars ([], [])
        in
        let unival_fn_lval = "UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ lval in
        causal_map := CausalMap.add unival_fn_lval exp_vars_with_ver !causal_map;
        let unival_fn_exp_vars =
          VarMap.fold
            (fun v _ fevs ->
              VarSet.add ("UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ v) fevs)
            exp_vars VarSet.empty
        in
        let unival_exp_vars =
          VarMap.fold
            (fun v _ fevs -> VarSet.add ("UNIVAL_" ^ v) fevs)
            exp_vars VarSet.empty
        in
        let new_var_ver =
          VarVerMap.mapi
            (fun v ver ->
              if VarSet.mem v unival_fn_exp_vars || VarSet.mem v unival_exp_vars
              then ver + 1
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
              let unival_fn_vname =
                "UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ vname
              in
              let unival_vname = "UNIVAL_" ^ vname in
              call_record (Cil.Var vi, Cil.NoOffset) vname
                (if VarMap.mem unival_fn_vname new_var_ver then
                 VarMap.find unival_fn_vname new_var_ver
                else VarMap.find unival_vname new_var_ver)
                loc
              @ rs)
            exp_vars []
        in
        var_ver := new_var_ver;
        result @ (instr :: (pred_record @ records)))
      else
        let unival_fn_lval = "UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ lval in
        let unival_lval = "UNIVAL_" ^ lval in
        let new_var_ver =
          if VarVerMap.mem unival_fn_lval !var_ver then
            VarVerMap.update unival_fn_lval
              (fun ver -> Some (Option.get ver + 1))
              !var_ver
          else if VarVerMap.mem unival_lval !var_ver then
            VarVerMap.update unival_lval
              (fun ver -> Some (Option.get ver + 1))
              !var_ver
          else VarVerMap.add unival_fn_lval 0 !var_ver
        in
        let exp_vars_with_ver =
          VarMap.fold
            (fun ev _ vs ->
              (* for debugging *)
              (* print_endline ev; *)
              let unival_fn_ev = "UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ ev in
              let unival_ev = "UNIVAL_" ^ ev in
              if VarVerMap.mem unival_fn_ev !var_ver then
                let ver = VarVerMap.find unival_fn_ev !var_ver in
                (unival_fn_ev ^ "_" ^ string_of_int ver) :: vs
              else if VarVerMap.mem unival_ev !var_ver then
                let ver = VarVerMap.find unival_ev !var_ver in
                (unival_ev ^ "_" ^ string_of_int ver) :: vs
              else (
                print_endline unival_fn_ev;
                raise (Failure "Not_Found_Var")))
            exp_vars []
        in
        (* for debugging *)
        (* print_endline "d"; *)
        let final_lval, ver_of_lval =
          if VarVerMap.mem unival_fn_lval new_var_ver then
            (unival_fn_lval, VarVerMap.find unival_fn_lval new_var_ver)
          else (unival_lval, VarVerMap.find unival_lval new_var_ver)
        in
        let lval_with_ver = final_lval ^ "_" ^ string_of_int ver_of_lval in
        causal_map := CausalMap.add lval_with_ver exp_vars_with_ver !causal_map;
        let lv_record = call_record lv lval ver_of_lval loc in
        var_ver := new_var_ver;
        result @ (instr :: lv_record)
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

  class funAssignVisitor (printf, flush, stream) faulty_func_list =
    object
      inherit Cil.nopCilVisitor

      method! vglob g =
        let loc = Cil.get_globalLoc g in
        if String.starts_with ~prefix:"/usr" loc.file then SkipChildren
        else DoChildren

      method! vfunc f =
        if
          String.length f.svar.vname >= 6
          && String.equal (String.sub f.svar.vname 0 6) "unival"
          || List.length faulty_func_list > 0
             && not (List.mem f.svar.vname faulty_func_list)
        then Cil.SkipChildren
        else (
          List.iter
            (fun form ->
              var_ver :=
                VarVerMap.add
                  ("UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ form.Cil.vname)
                  0 !var_ver)
            f.Cil.sformals;
          List.iter
            (fun local ->
              var_ver :=
                VarVerMap.add
                  ("UNIVAL_" ^ f.Cil.svar.vname ^ "_" ^ local.Cil.vname)
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

  let gsa_gen ?(faulty_func_list = []) work_dir origin_file_opt pt_file =
    Cil.resetCIL ();
    Cil.insertImplicitCasts := false;
    let cil_opt =
      try Some (Frontc.parse pt_file ()) with Frontc.ParseError _ -> None
    in
    if Option.is_none cil_opt then ()
    else
      let cil = Option.get cil_opt in
      let origin_file_cand = Filename.remove_extension pt_file ^ ".c" in
      let origin_file =
        if Sys.file_exists origin_file_cand then origin_file_cand
        else if Option.is_some origin_file_opt then Option.get origin_file_opt
        else (
          prerr_endline origin_file_cand;
          Utils.find_file (Filename.basename origin_file_cand) work_dir
          |> List.hd)
      in
      Logging.log "GSA_Gen %s (%s)" origin_file pt_file;
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
        cil.Cil.globals <- Cil.GVarDecl (stream, Cil.locUnknown) :: cil.globals;
        let global_vars = extract_gvar cil.Cil.globals in
        var_ver :=
          List.fold_left
            (fun vv gv -> VarVerMap.add ("UNIVAL_" ^ gv) 0 vv)
            VarVerMap.empty global_vars;
        Cil.visitCilFile
          (new funAssignVisitor (printf, flush, stream) faulty_func_list)
          cil;
        Unix.system
          ("cp " ^ origin_file ^ " "
          ^ Filename.remove_extension origin_file
          ^ ".origin.c")
        |> ignore;
        (if List.mem (Filename.basename origin_file) [ "proc_open.c"; "cast.c" ]
        then ()
        else
          let oc = open_out (Filename.remove_extension origin_file ^ ".c") in
          Cil.dumpFile !Cil.printerForMaincil oc "" cil;
          close_out oc);
        if
          List.mem
            (Filename.basename origin_file)
            [
              "gzip.c";
              "tif_unix.c";
              "http_auth.c";
              "main.c";
              "version.c";
              "grep.c";
              "readelf.c";
              "core_shntool.c";
              "sed.c";
              "tar.c";
            ]
        then append_constructor work_dir origin_file "output"

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
    let faulty_func_list =
      if !Cmdline.faulty_func then
        let ff_path = Filename.concat work_dir "faulty_func.txt" in
        let ic = open_in ff_path in
        let rec read_lines ic ffs =
          try
            let line = input_line ic in
            read_lines ic (line :: ffs)
          with End_of_file -> ffs
        in
        read_lines ic []
      else []
    in
    Utils.traverse_pp_file
      (fun pp_file ->
        let origin_file_opt = Utils.find_origin_file_opt pp_file in
        pp_file
        |> predicate_transform ~faulty_func_list
        |> gsa_gen ~faulty_func_list work_dir origin_file_opt)
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

  let printf_value_of printf stream loc name exp =
    Cil.Call
      ( None,
        Cil.Lval (Cil.Var printf, Cil.NoOffset),
        [
          Cil.Lval (Cil.Var stream, Cil.NoOffset);
          Cil.Const (Cil.CStr "%s:%d\n");
          Cil.Const (Cil.CStr name);
          exp;
        ],
        loc )

  let flush_value_of flush stream loc =
    Cil.Call
      ( None,
        Cil.Lval (Cil.Var flush, Cil.NoOffset),
        [ Cil.Lval (Cil.Var stream, Cil.NoOffset) ],
        loc )

  class instrumentVisitor printf flush stream =
    object
      inherit Cil.nopCilVisitor

      method! vglob g =
        let loc = Cil.get_globalLoc g in
        if String.starts_with ~prefix:"/usr" loc.file then SkipChildren
        else DoChildren

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
                        if not !Cmdline.no_seg then
                          let flush = flush_of flush stream loc in
                          i :: flush :: call :: is
                        else i :: call :: is)
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
                  if not !Cmdline.no_seg then
                    let flush =
                      flush_of flush stream loc |> Cil.mkStmtOneInstr
                    in
                    s :: flush :: call :: bstmts
                  else s :: call :: bstmts)
            [] blk.Cil.bstmts
          |> List.rev
        in
        blk.bstmts <- bstmts;
        Cil.DoChildren
    end

  let instrument work_dir origin_file_opt pt_file =
    Cil.resetCIL ();
    Cil.insertImplicitCasts := false;
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
      let origin_file_cand = Filename.remove_extension pt_file ^ ".c" in
      let origin_file =
        if Sys.file_exists origin_file_cand then origin_file_cand
        else Option.get origin_file_opt
      in
      Logging.log "Instrument Coverage %s (%s)" origin_file pt_file;
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
            (Unix.realpath origin_file)
            [
              "/experiment/src/gzip.c";
              "/experiment/src/libtiff/tif_unix.c";
              "/experiment/src/src/http_auth.c";
              "/experiment/src/main/main.c";
              "/experiment/src/version.c";
            ]
        then append_constructor work_dir origin_file "coverage"

  let read_value_map value_map_file =
    let value_map_json = Yojson.Basic.from_file value_map_file in
    Yojson.Basic.Util.to_assoc value_map_json
    |> List.map (fun (s, x) ->
           ( s,
             x |> Yojson.Basic.Util.to_assoc
             |> List.map (fun (s, x) ->
                    ( s,
                      x |> Yojson.Basic.Util.to_assoc
                      |> List.map (fun (s, x) ->
                             ( s,
                               x |> Yojson.Basic.Util.to_list
                               |> List.map (fun x ->
                                      x |> Yojson.Basic.Util.to_string
                                      |> int_of_string) )) )) ))

  class signalChecker signal_table file line score =
    object
      inherit Cil.nopCilVisitor

      method! vglob g =
        let loc = Cil.get_globalLoc g in
        if String.starts_with ~prefix:"/usr" loc.file then SkipChildren
        else DoChildren

      method! vfunc fd =
        if fd.Cil.svar.vname = "bugzoo_ctor" then SkipChildren else DoChildren

      method! vblock blk =
        let bstmts =
          List.fold_left
            (fun bstmts s ->
              match s.Cil.skind with
              | Cil.If (exp, thenb, elseb, ifloc) when ifloc.Cil.line = line ->
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  print_endline
                    ("ifloc line: " ^ ifloc.Cil.file ^ ":"
                    ^ (ifloc.Cil.line |> string_of_int));

                  Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () exp)
                  |> print_endline;

                  Hashtbl.add signal_table (file, line, score) true;

                  s :: bstmts
              | _ -> s :: bstmts)
            [] blk.Cil.bstmts
          |> List.rev
        in

        blk.bstmts <- bstmts;
        Cil.DoChildren
    end

  class conditionFinder printf flush stream assertion assume_line_list =
    object
      inherit Cil.nopCilVisitor

      method! vglob g =
        let loc = Cil.get_globalLoc g in
        if String.starts_with ~prefix:"/usr" loc.file then SkipChildren
        else DoChildren

      method! vfunc fd =
        if fd.Cil.svar.vname = "bugzoo_ctor" then SkipChildren else DoChildren

      method! vblock blk =
        let rec vname_of lv =
          match lv with
          | Cil.Var vi, Cil.NoOffset -> vi.Cil.vname
          | Cil.Var vi, Cil.Field (f, o) -> vi.Cil.vname ^ "." ^ f.Cil.fname
          | Cil.Var vi, Cil.Index (e, o) ->
              vi.Cil.vname ^ "["
              ^ Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () e)
              ^ "]"
          | Cil.Mem exp, Cil.NoOffset -> var_names_of exp |> List.hd |> fst
          | Cil.Mem exp, Cil.Field (f, o) ->
              (var_names_of exp |> List.hd |> fst) ^ "->" ^ f.Cil.fname
          | Cil.Mem exp, Cil.Index (e, o) -> failwith "5"
        and var_names_of exp =
          match exp with
          | Cil.Const c -> [ ("", None) ]
          | Cil.Lval lv -> [ (vname_of lv, Some lv) ]
          | Cil.SizeOfE e -> var_names_of e
          | Cil.AlignOfE e -> var_names_of e
          | Cil.UnOp (_, e, _) -> var_names_of e
          | Cil.BinOp (_, e1, e2, _) ->
              List.rev_append (var_names_of e1) (var_names_of e2)
          | Cil.Question (e1, e2, e3, _) ->
              List.rev_append
                (List.rev_append (var_names_of e1) (var_names_of e2))
                (var_names_of e3)
          | Cil.CastE (_, e) -> var_names_of e
          | _ -> []
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
          | Cil.TInt (Cil.ILongLong, _) -> "longlong"
          | Cil.TInt (Cil.IULongLong, _) -> "unsigned longlong"
          | Cil.TInt (Cil.IULong, _) -> "unsigned long"
          | Cil.TFloat (Cil.FFloat, _) -> "float"
          | Cil.TFloat (Cil.FDouble, _) -> "double"
          | Cil.TFloat (Cil.FLongDouble, _) -> "long double"
          | Cil.TPtr (Cil.TInt (Cil.IChar, _), _) -> "string"
          | Cil.TNamed (t, _) -> string_of_typ t.ttype
          | Cil.TComp (_, _) -> "TComp"
          | Cil.TEnum (_, _) -> "TEnum"
          | Cil.TBuiltin_va_list _ -> "TBuiltin_va_list"
          | Cil.TInt (_, _) -> "TInt"
          | Cil.TPtr (_, _) -> "TPtr"
          | Cil.TVoid _ -> "TVoid"
          | Cil.TArray (_, _, _) -> "TArray"
          | Cil.TFun (_, _, _, _) -> "TFun"
        in
        let bstmts, conditions =
          List.fold_left
            (fun (bstmts, conds) s ->
              match s.Cil.skind with
              | Cil.If (exp, thenb, elseb, ifloc)
                when List.mem ifloc.Cil.line assume_line_list ->
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  print_endline
                    ("ifloc line: " ^ ifloc.Cil.file ^ ":"
                    ^ (ifloc.Cil.line |> string_of_int));
                  (* print_endline ("loc line: " ^ (loc.Cil.line |> string_of_int)); *)
                  Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () exp)
                  |> print_endline;
                  let vars = try var_names_of exp with Failure _ -> [] in

                  (* let print_stmt, flush_stmt = call_record  *)
                  let call_record var vname loc =
                    let call_printf varname typ var_exp =
                      let fmt =
                        match typ with
                        | "char" | "signed char" | "unsigned char" -> "%c"
                        | "unsigned int" -> "%u"
                        | "int" | "short" -> "%d"
                        | "unsigned short" -> "%hd"
                        | "long" -> "%lld"
                        | "unsigned long" -> "%llu"
                        | "longlong" -> "%ld"
                        | "unsigned longlong" -> "%lu"
                        | "float" -> "%f"
                        | "double" | "long double" -> "%lf"
                        | "string" -> "string"
                        | s -> "NA:" ^ s
                      in
                      Cil.Call
                        ( None,
                          Cil.Lval (Cil.Var printf, Cil.NoOffset),
                          [
                            Cil.Lval (Cil.Var stream, Cil.NoOffset);
                            Cil.Const
                              (Cil.CStr
                                 (Printf.sprintf "%s" ifloc.Cil.file
                                 ^ ":"
                                 ^ (ifloc.Cil.line |> string_of_int)
                                 ^ "," ^ varname ^ "," ^ fmt ^ "\n"));
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
                    let t = string_of_typ (Cil.typeOfLval var) in
                    (call_printf vname t (Lval var), call_flush loc)
                  in

                  List.fold_left
                    (fun (bstmts, conds) (vname, var) ->
                      (* call_record var vname loc |> ignore; *)
                      match var with
                      | Some v ->
                          let print_instr, flush_instr =
                            call_record v vname loc
                          in
                          ( (flush_instr |> Cil.mkStmtOneInstr)
                            :: (print_instr |> Cil.mkStmtOneInstr)
                            (* :: (assertion_instr |> Cil.mkStmtOneInstr) *)
                            :: bstmts,
                            conds )
                      | None ->
                          (*failwith "condition has constant"*) (bstmts, conds))
                    (bstmts, exp :: conds)
                    vars
                  |> fun (bstmts, conds) -> (s :: bstmts, conds)
              | _ -> (s :: bstmts, conds))
            ([], []) blk.Cil.bstmts
          |> fun (bstmts, conds) -> (bstmts |> List.rev, conds |> List.rev)
        in

        (* List.iter
           (fun cond ->
             List.iter
               (fun (vname, exp) -> print_endline ("vname: " ^ vname))
               (var_names_of cond))
           conditions; *)
        blk.bstmts <- bstmts;
        Cil.DoChildren
    end

  class conditionCollector printf flush stream line condition_table =
    object
      inherit Cil.nopCilVisitor

      method! vglob g =
        let loc = Cil.get_globalLoc g in
        if String.starts_with ~prefix:"/usr" loc.file then SkipChildren
        else DoChildren

      method! vfunc fd =
        if fd.Cil.svar.vname = "bugzoo_ctor" then SkipChildren else DoChildren

      method! vblock blk =
        let rec vname_of lv =
          match lv with
          | Cil.Var vi, Cil.NoOffset -> vi.Cil.vname
          | Cil.Var vi, Cil.Field (f, o) -> vi.Cil.vname ^ "." ^ f.Cil.fname
          | Cil.Var vi, Cil.Index (e, o) ->
              vi.Cil.vname ^ "["
              ^ Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () e)
              ^ "]"
          | Cil.Mem exp, Cil.NoOffset -> var_names_of exp |> List.hd |> fst
          | Cil.Mem exp, Cil.Field (f, o) ->
              (var_names_of exp |> List.hd |> fst) ^ "->" ^ f.Cil.fname
          | Cil.Mem exp, Cil.Index (e, o) -> failwith "5"
        and var_names_of exp =
          match exp with
          | Cil.Const c -> [ ("", None) ]
          | Cil.Lval lv -> [ (vname_of lv, Some lv) ]
          | Cil.SizeOfE e -> var_names_of e
          | Cil.AlignOfE e -> var_names_of e
          | Cil.UnOp (_, e, _) -> var_names_of e
          | Cil.BinOp (_, e1, e2, _) ->
              List.rev_append (var_names_of e1) (var_names_of e2)
          | Cil.Question (e1, e2, e3, _) ->
              List.rev_append
                (List.rev_append (var_names_of e1) (var_names_of e2))
                (var_names_of e3)
          | Cil.CastE (_, e) -> var_names_of e
          | _ -> []
        in
        let bstmts, conditions =
          List.fold_left
            (fun (bstmts, conds) s ->
              match s.Cil.skind with
              | Cil.If (exp, thenb, elseb, ifloc) when ifloc.Cil.line = line ->
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  print_endline
                    ("ifloc line: " ^ ifloc.Cil.file ^ ":"
                    ^ (ifloc.Cil.line |> string_of_int));
                  (* print_endline ("loc line: " ^ (loc.Cil.line |> string_of_int)); *)
                  Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () exp)
                  |> print_endline;
                  let vars = var_names_of exp in
                  (* let print_stmt, flush_stmt = call_record  *)
                  List.fold_left
                    (fun (bstmts, conds) (vname, var) ->
                      (* call_record var vname loc |> ignore; *)
                      match var with
                      | Some v -> (bstmts, conds)
                      | None -> (bstmts, conds))
                    (bstmts, exp :: conds)
                    vars
                  |> fun (bstmts, conds) -> (s :: bstmts, conds)
              | _ -> (s :: bstmts, conds))
            ([], []) blk.Cil.bstmts
          |> fun (bstmts, conds) -> (bstmts |> List.rev, conds |> List.rev)
        in

        List.iter
          (fun cond ->
            List.iter
              (fun (vname, exp) ->
                print_endline ("vname: " ^ vname);
                if Hashtbl.mem condition_table vname then ()
                else Hashtbl.add condition_table vname exp)
              (var_names_of cond))
          conditions;

        Cil.DoChildren
    end

  class assertInjector printf flush stream assertion line condition_table
    value_map =
    object
      inherit Cil.nopCilVisitor

      method! vglob g =
        let loc = Cil.get_globalLoc g in
        if String.starts_with ~prefix:"/usr" loc.file then SkipChildren
        else DoChildren

      method! vfunc fd =
        if fd.Cil.svar.vname = "bugzoo_ctor" then SkipChildren else DoChildren

      method! vblock blk =
        let rec vname_of lv =
          match lv with
          | Cil.Var vi, Cil.NoOffset -> vi.Cil.vname
          | Cil.Var vi, Cil.Field (f, o) -> vi.Cil.vname ^ "." ^ f.Cil.fname
          | Cil.Var vi, Cil.Index (e, o) ->
              vi.Cil.vname ^ "["
              ^ Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () e)
              ^ "]"
          | Cil.Mem exp, Cil.NoOffset -> var_names_of exp |> List.hd |> fst
          | Cil.Mem exp, Cil.Field (f, o) ->
              (var_names_of exp |> List.hd |> fst) ^ "->" ^ f.Cil.fname
          | Cil.Mem exp, Cil.Index (e, o) -> failwith "5"
        and var_names_of exp =
          match exp with
          | Cil.Const c -> [ ("", None) ]
          | Cil.Lval lv -> [ (vname_of lv, Some lv) ]
          | Cil.SizeOfE e -> var_names_of e
          | Cil.AlignOfE e -> var_names_of e
          | Cil.UnOp (_, e, _) -> var_names_of e
          | Cil.BinOp (_, e1, e2, _) ->
              List.rev_append (var_names_of e1) (var_names_of e2)
          | Cil.Question (e1, e2, e3, _) ->
              List.rev_append
                (List.rev_append (var_names_of e1) (var_names_of e2))
                (var_names_of e3)
          | Cil.CastE (_, e) -> var_names_of e
          | _ -> []
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
          | Cil.TInt (Cil.ILongLong, _) -> "longlong"
          | Cil.TInt (Cil.IULongLong, _) -> "unsigned longlong"
          | Cil.TInt (Cil.IULong, _) -> "unsigned long"
          | Cil.TFloat (Cil.FFloat, _) -> "float"
          | Cil.TFloat (Cil.FDouble, _) -> "double"
          | Cil.TFloat (Cil.FLongDouble, _) -> "long double"
          | Cil.TPtr (Cil.TInt (Cil.IChar, _), _) -> "string"
          | Cil.TNamed (t, _) -> string_of_typ t.ttype
          | Cil.TComp (_, _) -> "TComp"
          | Cil.TEnum (_, _) -> "TEnum"
          | Cil.TBuiltin_va_list _ -> "TBuiltin_va_list"
          | Cil.TInt (_, _) -> "TInt"
          | Cil.TPtr (_, _) -> "TPtr"
          | Cil.TVoid _ -> "TVoid"
          | Cil.TArray (_, _, _) -> "TArray"
          | Cil.TFun (_, _, _, _) -> "TFun"
        in
        let call_record var vname loc =
          let call_printf varname typ var_exp =
            let fmt =
              match typ with
              | "char" | "signed char" | "unsigned char" -> "%c"
              | "unsigned int" -> "%u"
              | "int" | "short" -> "%d"
              | "unsigned short" -> "%hd"
              | "long" -> "%lld"
              | "unsigned long" -> "%llu"
              | "longlong" -> "%ld"
              | "unsigned longlong" -> "%lu"
              | "float" -> "%f"
              | "double" | "long double" -> "%lf"
              | "string" -> "%s"
              | s -> "NA:" ^ s
            in
            Cil.Call
              ( None,
                Cil.Lval (Cil.Var printf, Cil.NoOffset),
                [
                  Cil.Lval (Cil.Var stream, Cil.NoOffset);
                  Cil.Const
                    (Cil.CStr (Printf.sprintf "%s" varname ^ "," ^ fmt ^ "\n"));
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
          let t = string_of_typ (Cil.typeOfLval var) in
          (call_printf vname t (Lval var), call_flush loc)
        in
        let bstmts, conditions =
          List.fold_left
            (fun (bstmts, conds) s ->
              match s.Cil.skind with
              | Cil.If (exp, thenb, elseb, ifloc) when ifloc.Cil.line = line ->
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  print_endline
                    ("ifloc line: " ^ ifloc.Cil.file ^ ":"
                    ^ (ifloc.Cil.line |> string_of_int));
                  (* print_endline ("loc line: " ^ (loc.Cil.line |> string_of_int)); *)
                  Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () exp)
                  |> print_endline;
                  let vars = var_names_of exp in
                  (* let print_stmt, flush_stmt = call_record  *)
                  let assertion_neg =
                    List.fold_left
                      (fun conds (negt, vmap) ->
                        List.fold_left
                          (fun acc (vname, values) ->
                            print_endline vname;
                            List.fold_left
                              (fun cond_list value ->
                                Cil.BinOp
                                  ( Cil.Eq,
                                    Cil.Lval
                                      (Hashtbl.find condition_table vname
                                      |> Option.get),
                                    Cil.Const
                                      (Cil.CInt64
                                         (value |> Int64.of_int, Cil.IInt, None)),
                                    Cil.TInt (Cil.IInt, []) )
                                :: cond_list)
                              [] values
                            :: acc)
                          [] vmap
                        :: conds)
                      []
                      (value_map |> List.assoc "neg")
                  in
                  (* let assertion_pos =
                       List.fold_left
                         (fun conds (post, vmap) ->
                           List.fold_left
                             (fun acc (vname, values) ->
                               print_endline vname;
                               List.fold_left
                                 (fun cond_list value ->
                                   Cil.BinOp
                                     ( Cil.Eq,
                                       Cil.Lval
                                         (Hashtbl.find condition_table vname
                                         |> Option.get),
                                       Cil.Const
                                         (Cil.CInt64
                                            (value |> Int64.of_int, Cil.IInt, None)),
                                       Cil.TInt (Cil.IInt, []) )
                                   :: cond_list)
                                 [] values
                               :: acc)
                             [] vmap
                           :: conds)
                         []
                         (value_map |> List.assoc "pos")
                     in *)
                  let tr = Cil.Const (Cil.CInt64 (1L, Cil.IInt, None)) in
                  let fl = Cil.Const (Cil.CInt64 (0L, Cil.IInt, None)) in
                  let assertion_neg_exp =
                    List.fold_left
                      (fun acc conds ->
                        Cil.BinOp
                          ( Cil.LAnd,
                            acc,
                            Cil.UnOp
                              ( Cil.LNot,
                                List.fold_left
                                  (fun acc2 conds2 ->
                                    Cil.BinOp
                                      ( Cil.LAnd,
                                        acc2,
                                        List.fold_left
                                          (fun acc3 cond ->
                                            Cil.BinOp
                                              ( Cil.LOr,
                                                acc3,
                                                cond,
                                                Cil.TInt (Cil.IInt, []) ))
                                          fl conds2,
                                        Cil.TInt (Cil.IInt, []) ))
                                  tr conds,
                                Cil.TInt (Cil.IInt, []) ),
                            Cil.TInt (Cil.IInt, []) ))
                      tr assertion_neg
                  in
                  (* let assertion_pos_exp =
                       List.fold_left
                         (fun acc conds ->
                           Cil.BinOp
                             ( Cil.LOr,
                               acc,
                               List.fold_left
                                 (fun acc2 conds2 ->
                                   Cil.BinOp
                                     ( Cil.LAnd,
                                       acc2,
                                       List.fold_left
                                         (fun acc3 cond ->
                                           Cil.BinOp
                                             ( Cil.LOr,
                                               acc3,
                                               cond,
                                               Cil.TInt (Cil.IInt, []) ))
                                         fl conds2,
                                       Cil.TInt (Cil.IInt, []) ))
                                 tr conds,
                               Cil.TInt (Cil.IInt, []) ))
                         fl assertion_pos
                     in *)
                  (* let assertion_exp =
                       Cil.BinOp
                         ( Cil.LAnd,
                           assertion_neg_exp,
                           assertion_pos_exp,
                           Cil.TInt (Cil.IInt, []) )
                     in *)
                  let assertion_instr =
                    Cil.Call
                      ( None,
                        Cil.Lval (Cil.Var assertion, Cil.NoOffset),
                        (* [ Cil.Const (Cil.CInt64 (1L, Cil.IInt, None)) ], *)
                        [ assertion_neg_exp ],
                        loc )
                  in
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  let call =
                    printf_of printf stream loc |> Cil.mkStmtOneInstr
                  in
                  if not !Cmdline.no_seg then
                    let flush =
                      flush_of flush stream loc |> Cil.mkStmtOneInstr
                    in
                    ( (*(flush_instr |> Cil.mkStmtOneInstr)
                        :: (print_instr |> Cil.mkStmtOneInstr)
                        :: *)
                      s :: flush :: call
                      :: (assertion_instr |> Cil.mkStmtOneInstr)
                      :: bstmts,
                      conds )
                  else
                    ( (*(flush_instr |> Cil.mkStmtOneInstr)
                          :: (print_instr |> Cil.mkStmtOneInstr)
                          :: *)
                      s :: call
                      :: (assertion_instr |> Cil.mkStmtOneInstr)
                      :: bstmts,
                      conds )
              | Cil.Instr insts ->
                  let new_insts =
                    List.fold_left
                      (fun is i ->
                        let loc = Cil.get_instrLoc i in
                        let call = printf_of printf stream loc in
                        if not !Cmdline.no_seg then
                          let flush = flush_of flush stream loc in
                          i :: flush :: call :: is
                        else i :: call :: is)
                      [] insts
                    |> List.rev
                  in
                  s.skind <- Cil.Instr new_insts;
                  (s :: bstmts, conds)
              | _ ->
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  let call =
                    printf_of printf stream loc |> Cil.mkStmtOneInstr
                  in
                  if not !Cmdline.no_seg then
                    let flush =
                      flush_of flush stream loc |> Cil.mkStmtOneInstr
                    in
                    (s :: flush :: call :: bstmts, conds)
                  else (s :: call :: bstmts, conds))
            ([], []) blk.Cil.bstmts
          |> fun (bstmts, conds) -> (bstmts |> List.rev, conds |> List.rev)
        in

        List.iter
          (fun cond ->
            List.iter
              (fun (vname, exp) ->
                print_endline ("vname: " ^ vname);
                if Hashtbl.mem condition_table vname then ()
                else Hashtbl.add condition_table vname exp)
              (var_names_of cond))
          conditions;

        blk.bstmts <- bstmts;
        Cil.DoChildren
    end

  class assumeInjector printf flush stream assume_line_list condition_table
    is_pos =
    object
      inherit Cil.nopCilVisitor

      method! vglob g =
        let loc = Cil.get_globalLoc g in
        if String.starts_with ~prefix:"/usr" loc.file then SkipChildren
        else DoChildren

      method! vfunc fd =
        if fd.Cil.svar.vname = "bugzoo_ctor" then SkipChildren else DoChildren

      method! vblock blk =
        let bstmts =
          List.fold_left
            (fun bstmts s ->
              match s.Cil.skind with
              | Cil.If (exp, thenb, elseb, ifloc)
                when List.mem ifloc.Cil.line assume_line_list ->
                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  print_endline
                    ("ifloc line: " ^ ifloc.Cil.file ^ ":"
                    ^ (ifloc.Cil.line |> string_of_int));
                  (* failwith "assume if"; *)
                  Pretty.sprint 10 (Cil.printExp Cil.defaultCilPrinter () exp)
                  |> print_endline;

                  let tr = Cil.Const (Cil.CInt64 (1L, Cil.IInt, None)) in
                  let fl = Cil.Const (Cil.CInt64 (0L, Cil.IInt, None)) in
                  let new_s =
                    Cil.If ((if is_pos then tr else fl), thenb, elseb, ifloc)
                    |> Cil.mkStmt
                  in

                  let loc = Cil.get_stmtLoc s.Cil.skind in
                  let call =
                    printf_of printf stream loc |> Cil.mkStmtOneInstr
                  in

                  if not !Cmdline.no_seg then
                    let flush =
                      flush_of flush stream loc |> Cil.mkStmtOneInstr
                    in
                    new_s :: flush :: call :: bstmts
                  else new_s :: call :: bstmts
              | Cil.Instr insts ->
                  let new_insts =
                    List.fold_left
                      (fun is i ->
                        let loc = Cil.get_instrLoc i in
                        let call = printf_of printf stream loc in
                        if not !Cmdline.no_seg then
                          let flush = flush_of flush stream loc in
                          i :: flush :: call :: is
                        else i :: call :: is)
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
                  if not !Cmdline.no_seg then
                    let flush =
                      flush_of flush stream loc |> Cil.mkStmtOneInstr
                    in
                    s :: flush :: call :: bstmts
                  else s :: call :: bstmts)
            [] blk.Cil.bstmts
          |> List.rev
        in

        (* List.iter
           (fun cond ->
             List.iter
               (fun (vname, exp) ->
                 print_endline ("vname: " ^ vname);
                 if Hashtbl.mem condition_table vname then ()
                 else Hashtbl.add condition_table vname exp)
               (var_names_of cond))
           conditions; *)
        (* let bstmts =
             List.fold_left
               (fun bstmts s ->
                 match s.Cil.skind with
                 | Cil.Instr insts ->
                     let new_insts =
                       List.fold_left
                         (fun is i ->
                           let loc = Cil.get_instrLoc i in
                           let call = printf_of printf stream loc in
                           if not !Cmdline.no_seg then
                             let flush = flush_of flush stream loc in
                             i :: flush :: call :: is
                           else i :: call :: is)
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
                     if not !Cmdline.no_seg then
                       let flush =
                         flush_of flush stream loc |> Cil.mkStmtOneInstr
                       in
                       s :: flush :: call :: bstmts
                     else s :: call :: bstmts)
               [] blk.Cil.bstmts
             |> List.rev
           in *)
        blk.bstmts <- bstmts;
        Cil.DoChildren
    end

  let signal_checker work_dir origin_file_opt pt_file signal_table signal_list =
    Cil.resetCIL ();
    Cil.insertImplicitCasts := false;
    let cil_opt =
      print_endline pt_file;
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
      let origin_file_cand = Filename.remove_extension pt_file ^ ".c" in
      let origin_file =
        if Sys.file_exists origin_file_cand then origin_file_cand
        else Option.get origin_file_opt
      in
      Logging.log "Instrument Value Printer %s (%s)" origin_file pt_file;
      (* TODO: clean up *)
      Cil.visitCilFile (new findTypeVisitor "_IO_FILE") cil;
      Cil.visitCilFile (new findGVarVisitor "stderr") cil;
      if Option.is_none !found_type || Option.is_none !found_gvar then ()
      else
        let fileptr = Cil.TPtr (Cil.TComp (Option.get !found_type, []), []) in
        let stream = Cil.makeGlobalVar "__inst_stream" fileptr in
        cil.globals <- Cil.GVarDecl (stream, Cil.locUnknown) :: cil.globals;

        (* List.iter
             (fun (file, line) -> print_endline (file ^ (line |> string_of_int)))
             signal_list;

           print_endline (List.length signal_list |> string_of_int); *)
        List.iter
          (fun ((file : string), line, score) ->
            if
              String.ends_with
                (file |> Filename.basename |> Filename.remove_extension)
                (origin_file |> Filename.basename |> Filename.remove_extension)
            then
              (* failwith ("check_signal origin_file: " ^ origin_file); *)
              Cil.visitCilFile
                (new signalChecker signal_table file line score)
                cil
            else ())
          signal_list

  (* List.iter
     (fun (file, line) -> print_endline (file ^ (line |> string_of_int)))
     signal_list_filter; *)

  (* print_endline (List.length signal_list_filter |> string_of_int);

     print_signal signal_list_filter; *)

  let value_printer work_dir origin_file_opt pt_file assume_file
      assume_line_list =
    Cil.resetCIL ();
    Cil.insertImplicitCasts := false;
    let cil_opt =
      print_endline pt_file;
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
      let origin_file_cand = Filename.remove_extension pt_file ^ ".c" in
      let origin_file =
        if Sys.file_exists origin_file_cand then origin_file_cand
        else Option.get origin_file_opt
      in
      Logging.log "Instrument Value Printer %s (%s)" origin_file pt_file;
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
        let assertion =
          Cil.findOrCreateFunc cil "assert"
            (Cil.TFun
               (Cil.voidType, Some [ ("condition", Cil.intType, []) ], false, []))
        in
        let flush =
          Cil.findOrCreateFunc cil "fflush"
            (Cil.TFun (Cil.voidType, Some [ ("stream", fileptr, []) ], false, []))
        in
        let stream = Cil.makeGlobalVar "__inst_stream" fileptr in
        cil.globals <- Cil.GVarDecl (stream, Cil.locUnknown) :: cil.globals;
        (* Cil.visitCilFile (new instrumentVisitor printf flush stream) cil; *)
        if
          String.ends_with
            (Filename.remove_extension assume_file)
            (origin_file |> Filename.remove_extension)
        then
          Cil.visitCilFile
            (new conditionFinder printf flush stream assertion assume_line_list)
            cil;
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
            (Unix.realpath origin_file)
            [
              "/experiment/src/gzip.c";
              "/experiment/src/libtiff/tif_unix.c";
              "/experiment/src/src/http_auth.c";
              "/experiment/src/main/main.c";
              "/experiment/src/version.c";
            ]
        then append_constructor work_dir origin_file "output"

  let condition_collector work_dir origin_file_opt pt_file =
    Cil.resetCIL ();
    Cil.insertImplicitCasts := false;
    let cil_opt =
      print_endline pt_file;
      try Some (Frontc.parse pt_file ()) with
      | Frontc.ParseError _ -> None
      | Stack_overflow ->
          Logging.log "%s" "Stack overflow";
          None
      | e ->
          Logging.log "%s" (Printexc.to_string e);
          None
    in
    let condition_table = Hashtbl.create 100 in
    (if Option.is_none cil_opt then ()
    else
      let cil = Option.get cil_opt in
      let origin_file_cand = Filename.remove_extension pt_file ^ ".c" in
      let origin_file =
        if Sys.file_exists origin_file_cand then origin_file_cand
        else Option.get origin_file_opt
      in
      Logging.log "Condition Collector for %s (%s)" origin_file pt_file;
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
        (* Cil.visitCilFile (new instrumentVisitor printf flush stream) cil; *)
        if
          String.ends_with
            (Filename.remove_extension !Cmdline.inject_file)
            (origin_file |> Filename.remove_extension)
        then
          Cil.visitCilFile
            (new conditionCollector
               printf flush stream !Cmdline.inject_line condition_table)
            cil;
        (* Hashtbl.iter
           (fun name exp -> print_endline ("Hashtbl vname: " ^ name))
           condition_table; *)
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
            (Unix.realpath origin_file)
            [
              "/experiment/src/gzip.c";
              "/experiment/src/libtiff/tif_unix.c";
              "/experiment/src/src/http_auth.c";
              "/experiment/src/main/main.c";
              "/experiment/src/version.c";
            ]
        then append_constructor work_dir origin_file "coverage");
    condition_table

  let injector work_dir origin_file_opt pt_file condition_table value_map
      assume_file (assume_list : int list) is_pos =
    Cil.resetCIL ();
    Cil.insertImplicitCasts := false;
    let cil_opt =
      print_endline pt_file;
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
      let origin_file_cand = Filename.remove_extension pt_file ^ ".c" in
      let origin_file =
        if Sys.file_exists origin_file_cand then origin_file_cand
        else Option.get origin_file_opt
      in
      Logging.log "Condition Injector for %s (%s)" origin_file pt_file;
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
        print_endline origin_file;

        if !Cmdline.instrument = Cmdline.AssertInject then (
          if
            String.ends_with
              (Filename.remove_extension !Cmdline.inject_file)
              (origin_file |> Filename.remove_extension)
          then
            let assertion =
              Cil.findOrCreateFunc cil "assert"
                (Cil.TFun
                   ( Cil.voidType,
                     Some [ ("condition", Cil.intType, []) ],
                     false,
                     [] ))
            in
            Cil.visitCilFile
              (new assertInjector
                 printf flush stream assertion !Cmdline.inject_line
                 condition_table value_map)
              cil)
        else if !Cmdline.instrument = Cmdline.AssumeInject then
          if
            String.ends_with
              (Filename.remove_extension assume_file)
              (origin_file |> Filename.remove_extension)
          then
            (* failwith ("assume_file: " ^ assume_file); *)
            Cil.visitCilFile
              (new assumeInjector
                 printf flush stream assume_list condition_table is_pos)
              cil;
        (* Hashtbl.iter
           (fun name exp -> print_endline ("Hashtbl vname: " ^ name))
           condition_table; *)
        (if List.mem (Filename.basename origin_file) [ "proc_open.c"; "cast.c" ]
        then ()
        else
          let oc = open_out origin_file in
          Cil.dumpFile !Cil.printerForMaincil oc "" cil;
          close_out oc;
          if
            String.ends_with
              (Filename.remove_extension !Cmdline.inject_file)
              (origin_file |> Filename.remove_extension)
          then
            Unix.system
              ("sed -i \"s/void assert(int condition ) ;/#include \
                <assert.h>/\" " ^ origin_file)
            |> ignore);
        if
          List.mem
            (Unix.realpath origin_file)
            [
              "/experiment/src/gzip.c";
              "/experiment/src/libtiff/tif_unix.c";
              "/experiment/src/src/http_auth.c";
              "/experiment/src/main/main.c";
              "/experiment/src/version.c";
            ]
        then append_constructor work_dir origin_file "coverage"

  let run work_dir src_dir =
    Utils.traverse_pp_file
      (fun pp_file ->
        if
          !Cmdline.instrument = Cmdline.AssertInject
          && String.ends_with
               (Filename.remove_extension !Cmdline.inject_file ^ ".i")
               pp_file
        then ()
        else if !Cmdline.instrument = Cmdline.AssumeInject then
          let rec read_signal_list sig_file sig_list =
            match input_line sig_file |> String.split_on_char ':' with
            | [ file; line ] ->
                read_signal_list sig_file
                  ((file, line |> int_of_string) :: sig_list)
            | exception End_of_file ->
                close_in sig_file;
                sig_list
            | _ -> read_signal_list sig_file sig_list
          in
          let signal_file = open_in (work_dir ^ "/signal_list.txt") in
          let signal_neg_file = open_in (work_dir ^ "/signal_neg_list.txt") in
          let assume_list = read_signal_list signal_file [] in
          let assume_neg_list = read_signal_list signal_neg_file [] in
          let assume_file_list = List.map (fun (file, _) -> file) assume_list in
          let assume_neg_file_list =
            List.map (fun (file, _) -> file) assume_neg_list
          in
          if
            List.exists
              (fun file ->
                String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
              assume_file_list
          then ()
          else if
            List.exists
              (fun file ->
                String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
              assume_neg_file_list
          then ()
          else
            let origin_file_opt = Utils.find_origin_file_opt pp_file in
            instrument work_dir origin_file_opt pp_file
        else
          let origin_file_opt = Utils.find_origin_file_opt pp_file in
          instrument work_dir origin_file_opt pp_file)
      src_dir

  let check_signal work_dir src_dir =
    let rec read_signal_list sig_file sig_list =
      match
        input_line sig_file |> String.split_on_char '\t'
        |> List.filter (fun s -> s <> "")
      with
      | [ signal; info ] -> (
          match signal |> String.split_on_char ':' with
          | [ file; line ] -> (
              match info |> String.split_on_char ' ' with
              | [ _; _; score ] ->
                  read_signal_list sig_file
                    ((file, line |> int_of_string, score |> float_of_string)
                    :: sig_list)
              | _ -> failwith "Invalid format")
          | _ -> failwith "Invalid format")
      | exception End_of_file ->
          close_in sig_file;
          sig_list
      | _ -> read_signal_list sig_file sig_list
    in

    let (signal_list : (string * int * float) list) =
      let sig_file = open_in (work_dir ^ "/signal_list.txt") in
      read_signal_list sig_file []
    in

    print_endline
      ("signal_list length: " ^ (List.length signal_list |> string_of_int));

    let out_sig_file = open_out (work_dir ^ "/signal_list_filter.txt") in

    let rec print_signal = function
      | [] -> close_out out_sig_file
      | (file, line) :: tl ->
          Printf.fprintf out_sig_file "%s:%d\t0 0\n" file line;
          print_signal tl
    in
    let signal_table = Hashtbl.create 100000 in

    Utils.traverse_pp_file
      (fun pp_file ->
        let origin_file_opt = Utils.find_origin_file_opt pp_file in
        signal_checker work_dir origin_file_opt pp_file signal_table signal_list)
      src_dir;

    print_endline (Hashtbl.length signal_table |> string_of_int);

    Hashtbl.iter
      (fun (file, line, score) _ ->
        print_endline (file ^ ": " ^ (line |> string_of_int));
        Printf.fprintf out_sig_file "%s:%d\t0 0 %f\n" file line score)
      signal_table;

    close_out out_sig_file

  let value_print work_dir src_dir =
    let rec read_signal_list sig_file sig_list =
      match input_line sig_file |> String.split_on_char ':' with
      | [ file; line ] ->
          read_signal_list sig_file ((file, line |> int_of_string) :: sig_list)
      | exception End_of_file ->
          close_in sig_file;
          sig_list
      | _ -> read_signal_list sig_file sig_list
    in
    let signal_file = open_in (work_dir ^ "/signal_list.txt") in
    let assume_list = read_signal_list signal_file [] in

    Utils.traverse_pp_file
      (fun pp_file ->
        let assume_line_list =
          List.filter
            (fun (file, line) ->
              String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
            assume_list
          |> List.map (fun (_, line) -> line)
        in
        let assume_file_list = List.map (fun (file, _) -> file) assume_list in
        let origin_file_cand = Filename.remove_extension pp_file ^ ".c" in
        let origin_file_opt = Utils.find_origin_file_opt pp_file in
        let origin_file =
          if Sys.file_exists origin_file_cand then origin_file_cand
          else Option.get origin_file_opt
        in
        if
          List.exists
            (fun file ->
              String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
            assume_file_list
        then
          value_printer work_dir origin_file_opt pp_file pp_file
            assume_line_list
        else if
          List.mem
            (Unix.realpath origin_file)
            [
              "/experiment/src/gzip.c";
              "/experiment/src/libtiff/tif_unix.c";
              "/experiment/src/src/http_auth.c";
              "/experiment/src/main/main.c";
              "/experiment/src/version.c";
            ]
        then append_constructor work_dir origin_file "output")
      src_dir

  let assertinject work_dir src_dir =
    Utils.traverse_pp_file
      (fun pp_file ->
        if
          String.ends_with
            (Filename.remove_extension !Cmdline.inject_file ^ ".i")
            pp_file
        then
          let origin_file_opt = Utils.find_origin_file_opt pp_file in
          let condition_table =
            condition_collector work_dir origin_file_opt pp_file
          in
          let value_map = read_value_map (work_dir ^ "/assertion.txt") in

          injector work_dir origin_file_opt pp_file condition_table value_map
            pp_file [] true
        else
          let origin_file_opt = Utils.find_origin_file_opt pp_file in
          instrument work_dir origin_file_opt pp_file)
      src_dir

  let assumeinject work_dir src_dir =
    let rec read_signal_list sig_file sig_list =
      match input_line sig_file |> String.split_on_char ':' with
      | [ file; line ] ->
          read_signal_list sig_file ((file, line |> int_of_string) :: sig_list)
      | exception End_of_file ->
          close_in sig_file;
          sig_list
      | _ -> read_signal_list sig_file sig_list
    in
    let signal_file = open_in (work_dir ^ "/signal_list.txt") in
    let signal_neg_file = open_in (work_dir ^ "/signal_neg_list.txt") in
    let assume_list = read_signal_list signal_file [] in
    let assume_neg_list = read_signal_list signal_neg_file [] in

    Utils.traverse_pp_file
      (fun pp_file ->
        let assume_line_list =
          List.filter
            (fun (file, line) ->
              String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
            assume_list
          |> List.map (fun (_, line) -> line)
        in
        let assume_neg_line_list =
          List.filter
            (fun (file, line) ->
              String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
            assume_neg_list
          |> List.map (fun (_, line) -> line)
        in
        let assume_file_list = List.map (fun (file, _) -> file) assume_list in
        let assume_neg_file_list =
          List.map (fun (file, _) -> file) assume_neg_list
        in
        if
          List.exists
            (fun file ->
              String.ends_with
                (file |> Filename.basename |> Filename.remove_extension)
                (pp_file |> Filename.basename |> Filename.remove_extension))
            assume_file_list
        then
          let origin_file_opt = Utils.find_origin_file_opt pp_file in
          let condition_table =
            condition_collector work_dir origin_file_opt pp_file
          in
          (* failwith ("pp_file: " ^ pp_file); *)
          injector work_dir origin_file_opt pp_file condition_table [] pp_file
            assume_line_list true
          (* instrument work_dir origin_file_opt pp_file *)
        else if
          List.exists
            (fun file ->
              String.ends_with
                (file |> Filename.basename |> Filename.remove_extension)
                (pp_file |> Filename.basename |> Filename.remove_extension))
            assume_neg_file_list
        then
          let origin_file_opt = Utils.find_origin_file_opt pp_file in
          let condition_table =
            condition_collector work_dir origin_file_opt pp_file
          in
          injector work_dir origin_file_opt pp_file condition_table [] pp_file
            assume_neg_line_list false
          (* instrument work_dir origin_file_opt pp_file *)
        else
          let origin_file_opt = Utils.find_origin_file_opt pp_file in
          instrument work_dir origin_file_opt pp_file)
      src_dir
  (* Utils.traverse_pp_file
     (fun pp_file ->
       if
         !Cmdline.instrument = Cmdline.AssertInject
         && String.ends_with
              (Filename.remove_extension !Cmdline.inject_file ^ ".i")
              pp_file
       then ()
       else if !Cmdline.instrument = Cmdline.AssumeInject then (
         let rec read_signal_list sig_file sig_list =
           match input_line sig_file |> String.split_on_char ':' with
           | [ file; line ] ->
               read_signal_list sig_file
                 ((file, line |> int_of_string) :: sig_list)
           | exception End_of_file ->
               close_in sig_file;
               sig_list
           | _ -> read_signal_list sig_file sig_list
         in
         let signal_file = open_in (work_dir ^ "/signal_list.txt") in
         let signal_neg_file = open_in (work_dir ^ "/signal_neg_list.txt") in
         let assume_list = read_signal_list signal_file [] in
         let assume_neg_list = read_signal_list signal_neg_file [] in
         let assume_file_list = List.map (fun (file, _) -> file) assume_list in
         let assume_neg_file_list =
           List.map (fun (file, _) -> file) assume_neg_list
         in
         if
           List.exists
             (fun file ->
               String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
             assume_file_list
         then
           let origin_file_opt = Utils.find_origin_file_opt pp_file in
           instrument work_dir origin_file_opt pp_file
         else if
           List.exists
             (fun file ->
               String.ends_with (Filename.remove_extension file ^ ".i") pp_file)
             assume_neg_file_list
         then
           let origin_file_opt = Utils.find_origin_file_opt pp_file in
           instrument work_dir origin_file_opt pp_file)
       else
         let origin_file_opt = Utils.find_origin_file_opt pp_file in
         instrument work_dir origin_file_opt pp_file)
     src_dir *)
end

let run work_dir =
  Cil.initCIL ();
  Cil.insertImplicitCasts := false;
  let src_dir = Filename.concat work_dir "src" in
  match !Cmdline.instrument with
  | Cmdline.DfSan -> DfSan.run work_dir src_dir
  | Cmdline.GSA -> GSA.run work_dir src_dir
  | Cmdline.Coverage -> Coverage.run work_dir src_dir
  | Cmdline.ValuePrint -> Coverage.value_print work_dir src_dir
  | Cmdline.AssertInject -> Coverage.assertinject work_dir src_dir
  | Cmdline.AssumeInject -> Coverage.assumeinject work_dir src_dir
  | Cmdline.Filter -> Coverage.check_signal work_dir src_dir
  | Cmdline.Nothing -> ()
