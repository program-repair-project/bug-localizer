module F = Format

type t = {
  program : string;
  compiler_type : string;
  test_cases : string list;
  test_time_limit : int;
}

let find : string -> Yojson.Safe.t -> Yojson.Safe.t =
 fun name -> function
  | `Assoc l -> List.find (function n, _ -> n = name) l |> snd
  | _ -> raise Not_found

let to_string = function `String s -> s | _ -> raise Not_found

let to_int = function `Int i -> i | _ -> raise Not_found

let compiler_type_of desc = find "compiler" desc |> find "type" |> to_string

let program_of desc = find "program" desc |> to_string

let test_cases_of desc =
  let test_info = find "test-harness" desc in
  let num_of_passing = find "passing" test_info |> to_int in
  let num_of_failing = find "failing" test_info |> to_int in
  List.init num_of_passing (fun n -> "p" ^ string_of_int (n + 1))
  @ List.init num_of_failing (fun n -> "n" ^ string_of_int (n + 1))

let test_limit_of desc = find "test-harness" desc |> find "time-limit" |> to_int

let read work_dir =
  let json =
    let fn = Filename.concat work_dir "bug_desc.json" in
    if Sys.file_exists fn then Yojson.Safe.from_file fn
    else
      let fn = Filename.concat "/bugfixer" "bug_desc.json" in
      if Sys.file_exists fn then Yojson.Safe.from_file fn
      else failwith "Bug description not found"
  in
  Logging.log "Bug desc: %a" Yojson.Safe.pp json;
  let program = program_of json in
  let compiler_type = compiler_type_of json in
  let test_cases = test_cases_of json in
  let test_time_limit = test_limit_of json in
  { program; compiler_type; test_cases; test_time_limit }

let pp_test_cases fmt l =
  F.fprintf fmt "[";
  List.iter (fun x -> F.fprintf fmt "%s," x) l;
  F.fprintf fmt "]"

let pp fmt desc =
  F.fprintf fmt
    "{program: %s, compiler_type: %s, test_cases: %a, test_time_limit: %d}"
    desc.program desc.compiler_type pp_test_cases desc.test_cases
    desc.test_time_limit
