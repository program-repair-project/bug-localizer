module F = Format

module BugLocation = struct
  type t = Cil.location * float

  let pp fmt (l, score) = F.fprintf fmt "%s:%d\t%f" l.Cil.file l.Cil.line score
end

let run work_dir =
  Logging.log "Start localization";
  let coverage = Coverage.run work_dir [ "p1"; "p2"; "n1" ] in
  List.fold_left
    (fun locs elem ->
      Coverage.StrMap.fold
        (fun file lines locs ->
          let new_locs =
            List.map (fun line -> ({ Cil.file; line; byte = 0 }, 0.0)) lines
          in
          locs @ new_locs)
        elem.Coverage.coverage locs)
    [] coverage

let print locations =
  let oc = Filename.concat !Cmdline.out_dir "result.txt" |> open_out in
  let fmt = F.formatter_of_out_channel oc in
  List.iter (fun l -> F.fprintf fmt "%a\n" BugLocation.pp l) locations;
  close_out oc
