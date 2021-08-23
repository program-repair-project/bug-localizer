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

module GSA = struct
  let pred_num = ref (-1)

  let new_pred () =
    pred_num := !pred_num + 1;
    print_endline (string_of_int !pred_num);
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
        let new_block = Cil.mkBlock new_stmts in
        ChangeTo new_block
    end

  class predicateVisitor =
    object
      inherit Cil.nopCilVisitor

      method! vfunc f =
        ChangeTo (Cil.visitCilFunction (new assignInitializer f) f)
    end

  let predicate_transform pp_file =
    let origin_file = Filename.remove_extension pp_file ^ ".c" in
    Logging.log "Predicate transform %s (%s)" origin_file pp_file;
    let cil = Frontc.parse pp_file () in
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

  class assignVisitor record_func pt_file f =
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
    let call_record var vname ver loc =
      let fun_name = f.Cil.svar.vname in
      Cil.Call
        ( None,
          Cil.Lval (Cil.Var record_func, Cil.NoOffset),
          [
            Cil.Const (CStr (Filename.remove_extension pt_file));
            Cil.Const (Cil.CStr fun_name);
            Cil.Const (Cil.CStr vname);
            Cil.Const (Cil.CInt64 (Int64.of_int ver, Cil.IInt, None));
            Cil.Lval var;
          ],
          loc )
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
              let ver = VarVerMap.find ev !var_ver in
              ( (ev ^ string_of_int ver) :: vs,
                (ev ^ string_of_int (ver + 1)) :: nvs ))
            exp_vars ([], [])
        in
        causal_map := CausalMap.add lval exp_vars_with_ver !causal_map;
        let new_var_ver =
          VarVerMap.mapi
            (fun v ver -> if VarMap.mem v exp_vars then ver + 1 else ver)
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
              call_record (Cil.Var vi, Cil.NoOffset) vname
                (VarMap.find vname new_var_ver)
                loc
              :: rs)
            exp_vars []
        in
        var_ver := new_var_ver;
        result @ instr :: pred_record :: records)
      else
        let new_var_ver =
          if VarVerMap.mem lval !var_ver then
            VarVerMap.update lval
              (fun ver -> Some (Option.get ver + 1))
              !var_ver
          else VarVerMap.add lval 0 !var_ver
        in
        let exp_vars_with_ver =
          VarMap.fold
            (fun ev _ vs ->
              let ver = VarVerMap.find ev !var_ver in
              (ev ^ string_of_int ver) :: vs)
            exp_vars []
        in
        let ver_of_lval = VarVerMap.find lval new_var_ver in
        let lval_with_ver = lval ^ string_of_int ver_of_lval in
        causal_map := CausalMap.add lval_with_ver exp_vars_with_ver !causal_map;
        let lv_record = call_record lv lval ver_of_lval loc in
        var_ver := new_var_ver;
        result @ [ instr; lv_record ]
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

  class funAssignVisitor record_func pt_file origin_var_ver =
    object
      inherit Cil.nopCilVisitor

      method! vfunc f =
        var_ver := origin_var_ver;
        List.iter
          (fun form -> var_ver := VarVerMap.add form.Cil.vname 0 !var_ver)
          f.Cil.sformals;
        ChangeTo
          (Cil.visitCilFunction (new assignVisitor record_func pt_file f) f)
    end

  let extract_gvar globals =
    List.filter_map
      (fun g ->
        match g with
        | Cil.GVarDecl (vi, _) | Cil.GVar (vi, _, _) -> Some vi.Cil.vname
        | _ -> None)
      globals

  let gsa_gen pt_file =
    let origin_file = Filename.remove_extension pt_file ^ ".c" in
    Logging.log "GSA_Gen %s (%s)" origin_file pt_file;
    let cil = Frontc.parse pt_file () in
    let global_vars = extract_gvar cil.Cil.globals in
    var_ver :=
      List.fold_left
        (fun vv gv -> VarVerMap.add gv 0 vv)
        VarVerMap.empty global_vars;
    let record_func =
      Cil.findOrCreateFunc cil "unival_record"
        (Cil.TFun (Cil.voidType, None, false, []))
    in
    Cil.visitCilFile (new funAssignVisitor record_func pt_file !var_ver) cil;
    let oc = open_out pt_file in
    Cil.dumpFile !Cil.printerForMaincil oc "" cil;
    close_out oc

  let rec traverse_pp_file f root_dir =
    let files = Sys.readdir root_dir in
    Array.iter
      (fun file ->
        let file_path = Filename.concat root_dir file in
        if Sys.is_directory file_path then traverse_pp_file f file_path
        else if Filename.extension file = ".i" then f file_path
        else ())
      files

  let run src_dir =
    traverse_pp_file
      (fun pp_file -> pp_file |> predicate_transform |> gsa_gen)
      src_dir
end

let run work_dir =
  Cil.initCIL ();
  let src_dir = Filename.concat work_dir "src" in
  match !Cmdline.instrument with
  | Cmdline.DfSan -> DfSan.run work_dir src_dir
  | Cmdline.GSA -> GSA.run src_dir
  | Cmdline.Nothing -> ()
