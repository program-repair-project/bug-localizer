let work_dir : string option ref = ref None

let out_dir = ref "localizer-out"

type instrument = DfSan | GSA | Nothing

let instrument = ref Nothing

let select_instrument s =
  match s with
  | "dfsan" -> instrument := DfSan
  | "gsa" -> instrument := GSA
  | _ -> failwith "Unknown instrument"

let skip_compile = ref false

type engine = Tarantula | Prophet | Jaccard | Ochiai | Dummy | UniVal | All

let engine = ref Dummy

let select_engine s =
  match s with
  | "tarantula" -> engine := Tarantula
  | "prophet" -> engine := Prophet
  | "jaccard" -> engine := Jaccard
  | "ochiai" -> engine := Ochiai
  | "dummy" -> engine := Dummy
  | "unival" -> engine := UniVal
  | "all" -> engine := All
  | _ -> failwith "Unknown engine"

let jobs = ref 0 (* i.e., #cpus *)

let blacklist = ref []

let gnu_source = ref false

let options =
  [
    ("-outdir", Arg.Set_string out_dir, "Output directory");
    ( "-instrument",
      Arg.String select_instrument,
      "Specify instrument method (default: Nothing)" );
    ("-skip_compile", Arg.Set skip_compile, "Skip compilation");
    ( "-engine",
      Arg.String select_engine,
      "Specify localization engine (default: Dummy)" );
    ("-j", Arg.Set_int jobs, "Number of parallel jobs for make (default: -j)");
    ( "-blacklist",
      Arg.String (fun x -> blacklist := x :: !blacklist),
      "Blacklist for instrumentation" );
    ( "-gnu_source",
      Arg.Set gnu_source,
      "Add #define _GNU_SOURCE when instrumentation for some programs (e.g., \
       gimp)" );
  ]

let parse_arg x =
  work_dir := Some x;
  ()
