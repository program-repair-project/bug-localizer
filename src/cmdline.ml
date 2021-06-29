let files : string list ref = ref []

let out_dir = ref "localizer-out"

let options = [ ("-outdir", Arg.Set_string out_dir, "") ]

let parse_arg x = files := x :: !files
