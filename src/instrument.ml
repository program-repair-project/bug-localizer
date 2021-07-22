module NodeInfo = struct
  type t = Yojson.Safe.t

  let cmd_of t =
    ((t |> function
      | `Assoc l -> List.assoc "cmd" l
      | _ -> failwith "Invalid format")
     |> function
     | `List l -> List.hd l
     | _ -> failwith "Invalid format")
    |> function
    | `String s -> s
    | _ -> failwith "Invalid format"

  let filename_of t =
    t
    |> (function
         | `Assoc l -> List.assoc "loc" l | _ -> failwith "Invalid format")
    |> (function `String s -> s | _ -> failwith "Invalid format")
    |> String.split_on_char ':' |> Fun.flip List.nth 0
end

module NodeInfoMap = struct
  module M = Map.Make (String)

  type t = NodeInfo.t M.t

  let empty = M.empty

  let add = M.add

  let find = M.find
end

module LineSet = Set.Make (String)
module FileToEdges = Map.Make (String)

let read_nodes file =
  let ic = open_in file in
  Yojson.Safe.from_channel ic
  |> (function
       | `Assoc l -> List.assoc "nodes" l | _ -> failwith "Invalid format")
  |> (function `Assoc l -> l | _ -> failwith "Invalid format")
  |> List.fold_left
       (fun map (name, info) -> NodeInfoMap.add name info map)
       NodeInfoMap.empty
  |> fun x ->
  close_in ic;
  x

let read_covered_lines file =
  let ic = open_in file in
  let rec loop lst =
    match input_line ic with
    | line -> (
        String.split_on_char '\t' line |> function
        | h :: _ -> loop (LineSet.add h lst)
        | _ -> failwith "Invalid format")
    | exception End_of_file -> lst
  in
  loop LineSet.empty |> fun x ->
  close_in ic;
  x

let read_duedges nodes file =
  let ic = open_in file in
  let rec loop map =
    match input_line ic with
    | line -> (
        String.split_on_char '\t' line |> function
        | src :: dst :: _ ->
            let file = NodeInfoMap.find src nodes |> NodeInfo.filename_of in
            FileToEdges.update file
              (function
                | None -> Some [ (src, dst) ] | Some l -> Some ((src, dst) :: l))
              map
            |> loop
        | _ -> failwith "Invalid format")
    | exception End_of_file -> map
  in
  loop FileToEdges.empty |> fun x ->
  close_in ic;
  x

let initialize work_dir =
  let result_file = Filename.concat work_dir "result.txt" in
  let sparrow_out_dir = Filename.concat work_dir "src/sparrow-out" in
  let node_file = Filename.concat sparrow_out_dir "node.json" in
  let duedge_file =
    Filename.concat sparrow_out_dir "interval/datalog/DUEdge.facts"
  in
  let nodes = read_nodes node_file in
  let lines = read_covered_lines result_file in
  let duedges = read_duedges nodes duedge_file in
  (nodes, lines, duedges)

let instrument file pp_file nodes edges =
  Logging.log "Instrument %s (%s)" file pp_file;
  let cil = Frontc.parse pp_file () in
  List.iter
    (fun (src, dst) ->
      let src_node, dst_node =
        (NodeInfoMap.find src nodes, NodeInfoMap.find dst nodes)
      in
      if NodeInfo.cmd_of src_node = "skip" || NodeInfo.cmd_of dst_node = "skip"
      then ()
      else Logging.log "Edge (%s, %s)" src dst;
      ())
    edges

let run work_dir =
  Cil.initCIL ();
  let nodes, lines, duedges = initialize work_dir in
  let src_dir = Filename.concat work_dir "src" in
  FileToEdges.iter
    (fun file edges ->
      if file = "" then ()
      else
        let name = Filename.remove_extension file in
        let pp_file = Filename.concat src_dir (name ^ ".i") in
        if Sys.file_exists pp_file then instrument file pp_file nodes edges
        else Logging.log "%s not found" file)
    duedges
