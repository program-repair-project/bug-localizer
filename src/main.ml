module F = Format

let initialize work_dir =
  let out_dir = Filename.concat work_dir !Cmdline.out_dir in
  (try Unix.mkdir out_dir 0o775 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  print_endline ("Logging to " ^ out_dir);
  Logging.log_file :=
    Filename.concat out_dir "log.txt" |> open_out |> Option.some;
  Logging.log_formatter :=
    Option.map F.formatter_of_out_channel !Logging.log_file

let main () =
  let usageMsg = "Usage: localizer [options] [work dir]" in
  Arg.parse Cmdline.options Cmdline.parse_arg usageMsg;
  match !Cmdline.work_dir with
  | None ->
      prerr_endline "Error: No work directory is given";
      exit 1
  | Some work_dir ->
      initialize work_dir;
      Instrument.run work_dir;
      Localizer.run work_dir

let _ = main ()
