module F = Format

module BugLocation = struct
  type t = Cil.location * float

  let pp fmt (l, score) = F.fprintf fmt "%s:%d\t%f" l.Cil.file l.Cil.line score
end

let run file = []

let print locations =
  let oc = !Cmdline.out_dir ^ "result.txt" |> open_out in
  let fmt = F.formatter_of_out_channel oc in
  List.iter (fun l -> F.fprintf fmt "%a\n" BugLocation.pp l) locations
