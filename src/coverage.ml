module F = Format
module StrMap = Map.Make (String)

(* Line-level coverage using gcov *)
module LineCoverage = struct
  (* reference: https://github.com/squaresLab/BugZoo/blob/a87f03b2e33c2097c21c0175e613f4e95d9825eb/bugzoo/core/coverage.py#L106 *)
  type elem = { test : string; coverage : int list StrMap.t; test_result : int }
  type t = elem list

  let empty = []

  type tree = E of Xmlm.tag * tree list | D of string

  let elem_of test = { test; coverage = StrMap.empty; test_result = 0 }

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

  let pp_elem fmt { test; coverage; _ } =
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
        Scenario.run_test scenario.test_script test |> ignore;
        compute_coverage scenario.coverage_data;
        update_coverage scenario.coverage_data test coverage)
      empty bug_desc.BugDesc.test_cases
end

(* Line-level coverage using our own implementation *)
module LineCoverage2 = struct
  include LineCoverage

  module IntSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

  (* reference: https://github.com/squaresLab/BugZoo/blob/a87f03b2e33c2097c21c0175e613f4e95d9825eb/bugzoo/core/coverage.py#L106 *)
  type elem_internal = {
    test : string;
    coverage_set : IntSet.t StrMap.t;
    test_result : int;
  }

  let elem_of test = { test; coverage_set = StrMap.empty; test_result = 0 }

  let elem_of_internal { test; coverage_set; test_result } =
    { test; coverage = StrMap.map IntSet.elements coverage_set; test_result }

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

  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

  let update_coverage coverage_data test test_result coverage =
    (* let data =
         try read_whole_file coverage_data |> String.split_on_char '\n'
         with Sys_error _ -> []
       in *)
    let cov_file = open_in coverage_data in
    let try_read () =
      try Some (input_line cov_file) with End_of_file -> None
    in
    let rec update_elem elem =
      match try_read () with
      | Some line ->
          (if List.mem line [ ""; "__START_NEW_EXECUTION__" ] then elem
          else
            let lst = String.split_on_char ':' line in
            try
              let filename, lineno =
                (lst |> List.hd, lst |> List.tl |> List.hd |> int_of_string)
              in
              {
                test;
                coverage_set =
                  StrMap.update filename
                    (function
                      | Some s -> Some (IntSet.add lineno s)
                      | None -> Some (IntSet.singleton lineno))
                    elem.coverage_set;
                test_result;
              }
            with _ -> elem)
          |> update_elem
      | None ->
          close_in cov_file;
          elem
    in
    (* let elem =
         List.fold_left
           (fun elem line ->
             if List.mem line [ ""; "__START_NEW_EXECUTION__" ] then elem
             else
               let lst = String.split_on_char ':' line in
               try
                 let filename, lineno =
                   (List.nth lst 0, List.nth lst 1 |> int_of_string)
                 in
                 {
                   elem with
                   coverage_set =
                     StrMap.update filename
                       (function
                         | Some s -> Some (IntSet.add lineno s)
                         | None -> Some (IntSet.singleton lineno))
                       elem.coverage_set;
                   test_result;
                 }
               with _ -> elem)
           (elem_of test) data
       in *)
    let elem = update_elem (elem_of test) in
    elem :: coverage

  let run work_dir bug_desc =
    let scenario = Scenario.init ~stdio_only:true work_dir in
    Unix.chdir scenario.work_dir;
    (* compile to extract *.i *)
    Scenario.compile scenario bug_desc.BugDesc.compiler_type;
    let src_dir = Filename.concat scenario.work_dir "src" in
    Instrument.run scenario.work_dir;
    Unix.chdir scenario.Scenario.work_dir;
    (* compile instrumented files *)
    Scenario.compile scenario bug_desc.BugDesc.compiler_type;
    Unix.chdir scenario.Scenario.work_dir;
    Logging.log "Start test";
    let _cov_path = Filename.concat scenario.work_dir "coverage.txt" in
    List.fold_left
      (fun coverage test ->
        let regexp_pos = Str.regexp "p.*" in
        let regexp_hundred = Str.regexp ".*00" in
        if
          (* Str.string_match regexp_pos test 0
             && not
                  ((String.sub test 1 (String.length test - 1) |> int_of_string)
                   mod 100
                  = 0)
             || *)
          (!Cmdline.instrument = Cmdline.AssertInject
          || !Cmdline.instrument = Cmdline.ValuePrint)
          && Str.string_match regexp_pos test 0
          (* Str.string_match regexp_pos test 0
             && not
                  (List.mem
                     (String.sub test 1 (String.length test - 1) |> int_of_string)
                     (* [2121; 2122; 2123; 2124; 2125; 2129; 2130; 2136; 2138; 2141; 2146; 2149; 2150; 2153; 2154; 2155] *)
                     [ 1885; 1889; 1892; 1898; 1924 ]) *)
        then coverage
        else
          let test_result = Scenario.run_test scenario.test_script test in
          Unix.system
            "cat /experiment/coverage_data/tmp/*.txt | awk '!seen[$0]++' | tr \
             -d '\\000' > /experiment/coverage_data/coverage.txt"
          |> ignore;
          Unix.system "rm -f /experiment/coverage_data/tmp/*.txt" |> ignore;
          let cur_cov_path =
            (* Filename.concat "coverage_data" ("coverage." ^ test ^ ".txt") *)
            Filename.concat "coverage_data" "coverage.txt"
          in
          (*Unix.system ("mv " ^ cov_path ^ " " ^ cur_cov_path) |> ignore;*)
          let test_coverage =
            update_coverage cur_cov_path test test_result coverage
          in
          let test_data = test_coverage |> List.hd in
          (* print_endline
             (test_data.test ^ ", " ^ (test_data.test_result |> string_of_int)); *)
          test_coverage)
      empty bug_desc.BugDesc.test_cases
    |> List.map elem_of_internal
end
