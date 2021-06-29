module E = Errormsg

let parseOneFile fname =
  if !Cilutil.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = Frontc.parse fname () in
  if not (Feature.enabled "epicenter") then Rmtmps.removeUnusedTemps cil;
  cil

let parse () =
  match List.map parseOneFile !Cmdline.files with
  | [ one ] -> one
  | [] ->
      prerr_endline "Error: No arguments are given";
      exit 1
  | files ->
      Mergecil.ignore_merge_conflicts := true;
      let merged = Stats.time "merge" (Mergecil.merge files) "merged" in
      if !E.hadErrors then E.s (E.error "There were errors during merging");
      merged

let makeCFGinfo f =
  ignore (Partial.calls_end_basic_blocks f);
  ignore (Partial.globally_unique_vids f);
  Cil.iterGlobals f (fun glob ->
      match glob with
      | Cil.GFun (fd, _) ->
          Cil.prepareCFG fd;
          (* jc: blockinggraph depends on this "true" arg *)
          ignore (Cil.computeCFGInfo fd true)
      | _ -> ());
  f
