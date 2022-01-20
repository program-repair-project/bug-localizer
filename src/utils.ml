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

let rec traverse_pp_file f root_dir =
  let files = Sys.readdir root_dir in
  Array.iter
    (fun file ->
      let file_path = Filename.concat root_dir file in
      if (Unix.lstat file_path).st_kind = Unix.S_LNK then ()
      else if Sys.is_directory file_path then
        if
          Filename.check_suffix file_path ".libs"
          || Filename.check_suffix file_path ".hg"
        then ()
        else traverse_pp_file f file_path
      else if Filename.extension file = ".i" then f file_path
      else ())
    files

let rec find_file filename root_dir =
  let files = Sys.readdir root_dir in
  Array.fold_left
    (fun paths file ->
      let file_path = Filename.concat root_dir file in
      if (Unix.lstat file_path).st_kind = Unix.S_LNK then paths
      else if Sys.is_directory file_path then
        if Filename.check_suffix file_path "mytest" then paths
        else paths @ find_file filename file_path
      else if Filename.basename file_path = Filename.basename filename then
        file_path :: paths
      else paths)
    [] files

let find_origin_file_opt pp_file =
  let ic = open_in pp_file in
  let line = input_line ic in
  assert (String.starts_with ~prefix:"# 1" line);
  let filename = String.split_on_char '"' line |> Fun.flip List.nth 1 in
  if Filename.is_relative filename then
    try Some (Unix.realpath filename) with _ -> None
  else Some filename
