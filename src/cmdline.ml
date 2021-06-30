let work_dir : string option ref = ref None

let out_dir = ref "localizer-out"

let options = [ ("-outdir", Arg.Set_string out_dir, "") ]

let parse_arg x =
  work_dir := Some x;
  ()
