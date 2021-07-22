module F = Format
module LineCoverage = Coverage.LineCoverage

module BugLocation = struct
  type t = Cil.location * float * float * float

  let pp fmt (l, score_neg, score_pos, score_time) =
    F.fprintf fmt "%s:%d\t%f %f %f" l.Cil.file l.Cil.line score_neg score_pos
      score_time
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
            List.map
              (fun line -> ({ Cil.file; line; byte = 0 }, 0.0, 0.0, 0.0))
              lines
          in
          locs @ new_locs)
        elem.LineCoverage.coverage locs)
    [] coverage

let prophet_localizer work_dir bug_desc =
  let coverage = LineCoverage.run work_dir bug_desc in
  Logging.log "Coverage: %a" LineCoverage.pp coverage;
  copy_src ();
  let locations =
    List.fold_left
      (fun locs elem ->
        let regexp_pos = Str.regexp "p.*" in
        Coverage.StrMap.fold
          (fun file lines locs ->
            let new_locs =
              if Str.string_match regexp_pos elem.LineCoverage.test 0 then
                List.map
                  (fun line -> ({ Cil.file; line; byte = 0 }, 0.0, 1.0, 0.0))
                  lines
              else
                List.map
                  (fun line -> ({ Cil.file; line; byte = 0 }, 1.0, 0.0, 0.0))
                  lines
            in
            locs @ new_locs)
          elem.LineCoverage.coverage locs)
      [] coverage
  in
  let table = Hashtbl.create 99999 in
  List.iter
    (fun (l, s1, s2, s3) ->
      match Hashtbl.find_opt table l with
      | Some (new_s1, new_s2, new_s3) ->
          Hashtbl.replace table l (s1 +. new_s1, s2 +. new_s2, s3 +. new_s3)
      | _ -> Hashtbl.add table l (s1, s2, s3))
    locations;
  let new_locations =
    List.map
      (fun (l, (s1, s2, s3)) -> (l, s1, s2, s3))
      (List.of_seq (Hashtbl.to_seq table))
  in
  List.stable_sort
    (fun (_, s11, s12, s13) (_, s21, s22, s23) ->
      if s21 -. s11 <> 0. then int_of_float (s21 -. s11)
      else if s12 -. s22 <> 0. then int_of_float (s12 -. s22)
      else int_of_float (s23 -. s13))
    new_locations

let run work_dir =
  Logging.log "Start localization";
  let bug_desc = BugDesc.read work_dir in
  Logging.log "Bug desc: %a" BugDesc.pp bug_desc;
  match !Cmdline.engine with
  | Cmdline.Tarantula -> failwith "Not implemented"
  | Cmdline.Prophet -> prophet_localizer work_dir bug_desc
  | Cmdline.Dummy -> dummy_localizer work_dir bug_desc

let print locations =
  let oc = Filename.concat !Cmdline.out_dir "result.txt" |> open_out in
  let fmt = F.formatter_of_out_channel oc in
  List.iter (fun l -> F.fprintf fmt "%a\n" BugLocation.pp l) locations;
  close_out oc
