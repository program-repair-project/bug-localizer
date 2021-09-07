let rec join strlist delimiter =
  match strlist with
  | [ hd ] -> hd
  | hd :: tl -> hd ^ delimiter ^ join tl delimiter
  | [] -> ""

let remove_unnec_file filename =
  if not (Sys.file_exists (Filename.remove_extension filename ^ ".c")) then ()
  else (
    print_endline ("Remove " ^ filename);
    Unix.create_process "rm" [| "rm"; "-f"; filename |] Unix.stdin Unix.stdout
      Unix.stderr
    |> ignore;
    Unix.wait () |> ignore)

let rec remove_temp_files root_dir =
  let files = Sys.readdir root_dir in
  Array.iter
    (fun file ->
      let file_path = Filename.concat root_dir file in
      print_endline file_path;
      if (Unix.lstat file_path).st_kind = Unix.S_LNK then ()
      else if Sys.is_directory file_path then
        if Filename.check_suffix file_path ".hg" then ()
        else remove_temp_files file_path
      else if
        List.mem (Filename.extension file)
          [ ".i"; ".lo"; ".s"; ".gcno"; ".o"; ".asm" ]
      then remove_unnec_file file_path
      else ())
    files

let dash2under_bar s = String.map (fun c -> if c = '-' then '_' else c) s