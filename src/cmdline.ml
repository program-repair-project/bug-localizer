let work_dir : string option ref = ref None

let out_dir = ref "localizer-out"

let skip_compile = ref false

let options =
  [
    ("-outdir", Arg.Set_string out_dir, "Output directory");
    ("-skip_compile", Arg.Set skip_compile, "Skip compilation");
  ]

let parse_arg x =
  work_dir := Some x;
  ()
