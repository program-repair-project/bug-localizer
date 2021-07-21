module F = Format
module LineCoverage = Coverage.LineCoverage

module BugLocation = struct
  type t = Cil.location * float

  let pp fmt (l, score) = F.fprintf fmt "%s:%d\t%f" l.Cil.file l.Cil.line score
end

let copy_src () =
  Unix.create_process "cp"
    [| "cp"; "-rf"; "src"; !Cmdline.out_dir |]
    Unix.stdin Unix.stdout Unix.stderr
  |> ignore;

  match Unix.wait () |> snd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> failwith ("Error " ^ string_of_int n ^ ": copy failed")
  | _ -> failwith "copy failed"

let dummy_localizer work_dir bug_desc =
  let coverage = LineCoverage.run work_dir bug_desc in
  Logging.log "Coverage: %a" LineCoverage.pp coverage;
  copy_src ();
  List.fold_left
    (fun locs elem ->
      Coverage.StrMap.fold
        (fun file lines locs ->
          let new_locs =
            List.map (fun line -> ({ Cil.file; line; byte = 0 }, 0.0)) lines
          in
          locs @ new_locs)
        elem.LineCoverage.coverage locs)
    [] coverage

let run work_dir =
  Logging.log "Start localization";
  let bug_desc = BugDesc.read work_dir in
  Logging.log "Bug desc: %a" BugDesc.pp bug_desc;
  match !Cmdline.engine with
  | Cmdline.Tarantula -> failwith "Not implemented"
  | Cmdline.Dummy -> dummy_localizer work_dir bug_desc

let print locations =
  let oc = Filename.concat !Cmdline.out_dir "result.txt" |> open_out in
  let fmt = F.formatter_of_out_channel oc in
  List.iter (fun l -> F.fprintf fmt "%a\n" BugLocation.pp l) locations;
  close_out oc
