let work_dir : string option ref = ref None

let out_dir = ref "localizer-out"

let instrument = ref false

let skip_compile = ref false

type engine = Tarantula | Prophet | Dummy

let engine = ref Dummy

let select_engine s =
  match s with
  | "tarantula" -> engine := Tarantula
  | "prophet" -> engine := Prophet
  | "dummy" -> engine := Dummy
  | _ -> failwith "Unknown engine"

let options =
  [
    ("-outdir", Arg.Set_string out_dir, "Output directory");
    ("-instrument", Arg.Set instrument, "Instrument based on Sparrow results");
    ("-skip_compile", Arg.Set skip_compile, "Skip compilation");
    ( "-engine",
      Arg.String select_engine,
      "Specify localization engine (default: Dummy)" );
  ]

let parse_arg x =
  work_dir := Some x;
  ()
