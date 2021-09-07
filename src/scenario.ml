type t = {
  work_dir : string;
  compile_script : string;
  test_script : string;
  coverage_data : string;
}

let instrument_code () =
  String.concat ""
    [
      "/* BUGZOO :: INSTRUMENTATION :: START */\n";
      (if !Cmdline.gnu_source then "#define _GNU_SOURCE\n" else "");
      "#include <stdio.h>\n";
      "#include <stdlib.h>\n";
      "#include <signal.h>\n";
      "#ifdef __cplusplus\n";
      "  extern \"C\" void __gcov_flush(void);\n";
      "#else\n";
      "  void __gcov_flush(void);\n";
      "#endif\n";
      "#ifndef BUGZOO_SIGHANDLER\n";
      "#define BUGZOO_SIGHANDLER 1\n";
      "static void bugzoo_sighandler(int sig){\n";
      "  __gcov_flush();\n";
      "  if(sig != SIGUSR1 && sig != SIGUSR2)\n";
      "    exit(1);\n";
      "}\n";
      "#endif\n";
      "#ifndef BUGZOO_CTOR\n";
      "#define BUGZOO_CTOR 1\n";
      "static void bugzoo_ctor (void) __attribute__ ((constructor));\n";
      "static void bugzoo_ctor (void) {\n";
      "  struct sigaction new_action;\n";
      "  new_action.sa_handler = bugzoo_sighandler;\n";
      "  sigemptyset(&new_action.sa_mask);\n";
      "  new_action.sa_flags = 0;\n";
      "  sigaction(SIGTERM, &new_action, NULL);\n";
      "  sigaction(SIGINT, &new_action, NULL);\n";
      "  sigaction(SIGKILL, &new_action, NULL);\n";
      "  sigaction(SIGSEGV, &new_action, NULL);\n";
      "  sigaction(SIGFPE, &new_action, NULL);\n";
      "  sigaction(SIGBUS, &new_action, NULL);\n";
      "  sigaction(SIGILL, &new_action, NULL);\n";
      "  sigaction(SIGABRT, &new_action, NULL);\n";
      "  /* Use signal for SIGUSR to remove handlers */\n";
      "  signal(SIGUSR1, bugzoo_sighandler);\n";
      "  signal(SIGUSR2, bugzoo_sighandler);\n";
      "}\n";
      "#endif\n";
      "/* BUGZOO :: INSTRUMENTATION :: END */\n";
    ]

let unival_record_code filename =
  String.concat ""
    [
      "/* UNIVAL :: INSTRUMENTATION :: START */\n";
      "#include <string.h>\n";
      "#include <stdarg.h>\n";
      "void unival_record_" ^ filename
      ^ "(char *filename, char *funcname, int line, char *varname, int \
         version, ...) {\n";
      "  va_list ap;\n";
      "  va_start(ap, version);\n";
      "  char *which_type = va_arg(ap, const char*);\n";
      "  if (strncmp(varname, \"OOJAHOOO_PRED\", 13) == 0) {\n";
      "    int i_val = va_arg(ap, int) == 0 ? 0 : 1;\n";
      "    printf(\"%s,%s,%d,%s,%d,%d\\n\", filename, funcname, line, varname, \
       version, i_val);\n";
      "  }\n";
      "  else {\n";
      "    if (strcmp(which_type, \"char\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%c\\n\", filename, funcname, line, \
       varname, version, (char) va_arg(ap, int));\n";
      "    } else if (strcmp(which_type, \"signed char\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%c\\n\", filename, funcname, line, \
       varname, version, (char) va_arg(ap, int));\n";
      "    } else if (strcmp(which_type, \"unsigned char\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%c\\n\", filename, funcname, line, \
       varname, version, (unsigned char) va_arg(ap, int));\n";
      "    } else if (strcmp(which_type, \"int\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%c\\n\", filename, funcname, line, \
       varname, version, va_arg(ap, int));\n";
      "    } else if (strcmp(which_type, \"unsigned int\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%u\\n\", filename, funcname, line, \
       varname, version, (unsigned int) va_arg(ap, int));\n";
      "    } else if (strcmp(which_type, \"short\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%d\\n\", filename, funcname, line, \
       varname, version, (short) va_arg(ap, int));\n";
      "    } else if (strcmp(which_type, \"unsigned short\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%hd\\n\", filename, funcname, line, \
       varname, version, (unsigned short) va_arg(ap, int));\n";
      "    } else if (strcmp(which_type, \"long\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%ld\\n\", filename, funcname, line, \
       varname, version, va_arg(ap, long));\n";
      "    } else if (strcmp(which_type, \"unsigned long\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%lu\\n\", filename, funcname, line, \
       varname, version, va_arg(ap, unsigned long));\n";
      "    } else if (strcmp(which_type, \"float\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%f\\n\", filename, funcname, line, \
       varname, version, (float) va_arg(ap, double));\n";
      "    } else if (strcmp(which_type, \"double\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%lf\\n\", filename, funcname, line, \
       varname, version, va_arg(ap, double));\n";
      "    } else if (strcmp(which_type, \"long double\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%lf\\n\", filename, funcname, line, \
       varname, version, va_arg(ap, long double));\n";
      "    } else if (strcmp(which_type, \"string\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,%s\\n\", filename, funcname, line, \
       varname, version, va_arg(ap, const char*));\n";
      "    } else if (strcmp(which_type, \"NA\") == 0) {\n";
      "      printf(\"%s,%s,%d,%s,%d,NA\\n\", filename, funcname, line, \
       varname, version);\n";
      "    }\n";
      "  }\n";
      "}\n";
      "/* UNIVAL :: INSTRUMENTATION :: END */\n";
    ]

let file_instrument filename src_dir =
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let c_code = read_whole_file filename in
  let unival_instr =
    if !Cmdline.engine = Cmdline.UniVal then
      let bn =
        Utils.dash2under_bar
          (Filename.remove_extension (Filename.basename filename))
      in
      (* let oc = open_out (Filename.concat src_dir ("unival_" ^ bn ^ ".h")) in
            Printf.fprintf oc
              "extern void unival_record_%s(char *filename, char *funcname, int \
               line, char *varname, int version, ...);\n"
              bn;
         close_out oc; *)
      unival_record_code bn
    else ""
  in
  let instr_c_code = instrument_code () ^ unival_instr ^ c_code in
  let oc = open_out filename in
  Printf.fprintf oc "%s" instr_c_code;
  close_out oc

let file_instrument_all work_dir =
  let rec traverse_file f root_dir =
    let files = Sys.readdir root_dir in
    Array.iter
      (fun file ->
        let file_path = Filename.concat root_dir file in
        if (Unix.lstat file_path).st_kind = Unix.S_LNK then ()
        else if List.mem file !Cmdline.blacklist then ()
        else if Sys.is_directory file_path then traverse_file f file_path
        else if Filename.extension file = ".c" then
          f file_path (Filename.concat work_dir "src")
        else ())
      files
  in
  traverse_file file_instrument work_dir

let init work_dir =
  let work_dir =
    if Filename.is_relative work_dir then
      Filename.concat (Unix.getcwd ()) work_dir
    else work_dir
  in
  file_instrument_all work_dir;
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
  let jobs =
    if !Cmdline.jobs = 0 then "-j" else "-j" ^ string_of_int !Cmdline.jobs
  in
  Unix.create_process "make" [| "make"; jobs |] Unix.stdin Unix.stdout
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
      "CFLAGS=--coverage --save-temps -Wno-error";
      "CXXFLAGS=--coverage --save-temps";
      "LDFLAGS=-lgcov --coverage";
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
