module F = Format
module StrMap = Map.Make (String)

(* Line-level coverage using gcov *)
module LineCoverage = struct
  (* reference: https://github.com/squaresLab/BugZoo/blob/a87f03b2e33c2097c21c0175e613f4e95d9825eb/bugzoo/core/coverage.py#L106 *)
  type elem = { test : string; coverage : int list StrMap.t }

  type t = elem list

  let empty = []

  type tree = E of Xmlm.tag * tree list | D of string

  let elem_of test = { test; coverage = StrMap.empty }

  let read_xml file =
    let ic = open_in file in
    let i = Xmlm.make_input (`Channel ic) in
    let el tag childs = E (tag, childs) in
    let data d = D d in
    Xmlm.input_doc_tree ~el ~data i

  let find_filename al =
    match
      List.find_map
        (function (_, "filename"), data -> Some data | _ -> None)
        al
    with
    | Some data -> data
    | _ -> failwith "Unknown filename"

  let elem_of_attr filename elem al =
    if List.exists (function (_, "hits"), hits -> hits <> "0" | _ -> false) al
    then
      match
        List.find_map
          (function
            | (_, "number"), data -> Some (int_of_string data) | _ -> None)
          al
      with
      | Some line ->
          {
            elem with
            coverage =
              StrMap.update filename
                (function Some l -> Some (line :: l) | None -> Some [ line ])
                elem.coverage;
          }
      | _ -> elem
    else elem

  let rec elem_of_xml ?(filename = "") elem xml =
    match xml with
    | E (((_, "coverage"), _), l) ->
        List.find
          (function E (((_, "packages"), _), _) -> true | _ -> false)
          l
        |> elem_of_xml elem
    | E (((_, "packages"), _), l)
    | E (((_, "package"), _), l)
    | E (((_, "classes"), _), l) ->
        List.fold_left (fun elem e -> elem_of_xml elem e) elem l
    | E (((_, "class"), al), cl) ->
        let filename = find_filename al in
        List.fold_left (fun elem e -> elem_of_xml ~filename elem e) elem cl
    | E (((_, "lines"), _), l) ->
        List.fold_left (fun elem e -> elem_of_xml ~filename elem e) elem l
    | E (((_, "line"), al), _) -> elem_of_attr filename elem al
    | _ -> elem

  let pp_lines fmt lines = List.iter (fun l -> F.fprintf fmt "%d, " l) lines

  let pp_coverage fmt cov =
    StrMap.iter
      (fun file lines -> F.fprintf fmt "%s: %a\n" file pp_lines lines)
      cov

  let pp_elem fmt { test; coverage } =
    F.fprintf fmt "test: %s\ncoverage:\n%a\n" test pp_coverage coverage

  let pp fmt cov = List.iter (fun elem -> pp_elem fmt elem) cov

  let compute_coverage coverage_data =
    if Sys.file_exists coverage_data then Unix.unlink coverage_data;
    Unix.create_process "gcovr"
      [| "gcovr"; "-o"; coverage_data; "-x"; "-d"; "-r"; "." |]
      Unix.stdin Unix.stdout Unix.stderr
    |> ignore;
    match Unix.wait () |> snd with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
        failwith ("Error " ^ string_of_int n ^ ": coverage failed")
    | _ -> failwith "Coverage failed"

  let update_coverage coverage_data test coverage =
    let xml = read_xml coverage_data |> snd in
    let elem = elem_of_xml (elem_of test) xml in
    elem :: coverage

  let run work_dir bug_desc =
    let scenario = Scenario.init work_dir in
    Unix.chdir scenario.work_dir;
    if not !Cmdline.skip_compile then (
      Logging.log "Start compile";
      Scenario.compile scenario bug_desc.BugDesc.compiler_type;
      Unix.chdir scenario.work_dir);
    Logging.log "Start test";
    List.fold_left
      (fun coverage test ->
        Scenario.run_test scenario.test_script test;
        compute_coverage scenario.coverage_data;
        update_coverage scenario.coverage_data test coverage)
      empty bug_desc.BugDesc.test_cases
end
