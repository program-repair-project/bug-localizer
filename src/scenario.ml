type t = {
  work_dir : string;
  compile_script : string;
  test_script : string;
  coverage_data : string;
}

let init work_dir =
  let work_dir =
    if Filename.is_relative work_dir then
      Filename.concat (Unix.getcwd ()) work_dir
    else work_dir
  in
  {
    work_dir;
    compile_script = Filename.concat work_dir "compile.sh";
    test_script = Filename.concat work_dir "test.sh";
    coverage_data = Filename.concat work_dir "coverage.xml";
  }

let simple_compiler compile_script =
  Unix.create_process compile_script [| compile_script |] Unix.stdin Unix.stdout
    Unix.stderr
  |> ignore;
  match Unix.wait () |> snd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      failwith ("Error " ^ string_of_int n ^ ": " ^ compile_script ^ " failed")
  | _ -> failwith (compile_script ^ " failed")

let make () =
  Unix.create_process "make" [| "make"; "-j" |] Unix.stdin Unix.stdout
    Unix.stderr
  |> ignore;
  match Unix.wait () |> snd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> failwith ("Error " ^ string_of_int n ^ ": make failed")
  | _ -> failwith "make failed"

let configure () =
  Unix.create_process "./configure"
    [|
      "./configure";
      "CFLAGS=--coverage --save-temps";
      "CXXFLAGS=--coverage --save-temps";
      "LDFLAGS=-lgcov";
    |]
    Unix.stdin Unix.stdout Unix.stderr
  |> ignore;
  match Unix.wait () |> snd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      failwith ("Error " ^ string_of_int n ^ ": configure failed")
  | _ -> failwith "configure failed"

let make_clean () =
  Unix.create_process "make" [| "make"; "clean" |] Unix.stdin Unix.stdout
    Unix.stderr
  |> ignore;
  match Unix.wait () |> snd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      failwith ("Error " ^ string_of_int n ^ ": make clean failed")
  | _ -> failwith "make clean failed"

let make_distclean () =
  Unix.create_process "make" [| "make"; "distclean" |] Unix.stdin Unix.stdout
    Unix.stderr
  |> ignore;
  match Unix.wait () |> snd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      failwith ("Error " ^ string_of_int n ^ ": make distclean failed")
  | _ -> failwith "make distclean failed"

let configure_and_make () =
  Unix.chdir "src";
  make_clean ();
  make_distclean ();
  configure ();
  make ()

let compile scenario compiler_type =
  match compiler_type with
  | "compile" -> simple_compiler scenario.compile_script
  | "configure-and-make" -> configure_and_make ()
  | _ -> failwith "Unknown compiler"

let run_test test_script name =
  Unix.create_process test_script [| test_script; name |] Unix.stdin Unix.stdout
    Unix.stderr
  |> ignore;
  Unix.wait () |> ignore
