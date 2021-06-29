module F = Format

let initialize () =
  (try Unix.mkdir !Cmdline.out_dir 0o775
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  print_endline ("Logging to " ^ !Cmdline.out_dir);
  Logging.log_file :=
    Filename.concat !Cmdline.out_dir "log.txt" |> open_out |> Option.some;
  Logging.log_formatter :=
    Option.map F.formatter_of_out_channel !Logging.log_file

let main () =
  let usageMsg = "Usage: bug-localizer [options] source-files" in
  Arg.parse Cmdline.options Cmdline.parse_arg usageMsg;
  let file = Frontend.parse () in
  let result = Localizer.run file in
  Localizer.print result

let _ = main ()
