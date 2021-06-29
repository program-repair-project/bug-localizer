module F = Format
module P = Printf

let log_file : out_channel option ref = ref None

let log_formatter = ref None

let string_of_current_time () =
  Unix.time () |> Unix.localtime |> fun tm ->
  P.sprintf "%d%02d%02d-%02d:%02d:%02d" (1900 + tm.tm_year) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let log fmt =
  match !log_formatter with
  | Some log_formatter ->
      F.fprintf log_formatter "[%s] " (string_of_current_time ());
      F.kfprintf
        (fun log_formatter ->
          F.fprintf log_formatter "\n";
          F.pp_print_flush log_formatter ())
        log_formatter fmt
  | None -> failwith "Cannot open logfile"
