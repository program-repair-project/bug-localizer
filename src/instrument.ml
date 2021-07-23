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

type dfsan_funs = {
  create_label : Cil.varinfo;
  set_label : Cil.varinfo;
  get_label : Cil.varinfo;
  has_label : Cil.varinfo;
}

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

let rec instrument_instr dfsan_funs edges instrs results =
  match instrs with
  | (Cil.Set ((Var vi, NoOffset), _, loc) as i) :: tl ->
      let name = Cil.mkString vi.vname in
      Cil.Call
        ( None,
          Cil.Lval (Cil.Var dfsan_funs.create_label, Cil.NoOffset),
          [ name; Cil.zero ],
          loc )
      :: i :: results
      |> instrument_instr dfsan_funs edges tl
  | i :: tl -> i :: results |> instrument_instr dfsan_funs edges tl
  | [] -> List.rev results

class assignVisitor dfsan_funs edges =
  object
    inherit Cil.nopCilVisitor

    method! vstmt s =
      match s.Cil.skind with
      | Cil.Instr i ->
          s.Cil.skind <- Cil.Instr (instrument_instr dfsan_funs edges i []);
          DoChildren
      | _ -> DoChildren
  end

let instrument file pp_file _ edges =
  Logging.log "Instrument %s (%s)" file pp_file;
  let cil = Frontc.parse pp_file () in
  let dfsan_funs =
    {
      create_label =
        Cil.findOrCreateFunc cil "dfsan_create_label"
          (Cil.TFun (Cil.voidType, None, false, []));
      set_label =
        Cil.findOrCreateFunc cil "dfsan_set_label"
          (Cil.TFun (Cil.voidType, None, false, []));
      get_label =
        Cil.findOrCreateFunc cil "dfsan_get_label"
          (Cil.TFun (Cil.voidType, None, false, []));
      has_label =
        Cil.findOrCreateFunc cil "dfsan_has_label"
          (Cil.TFun (Cil.voidType, None, false, []));
    }
  in
  Cil.visitCilFile (new assignVisitor dfsan_funs edges) cil;
  let oc = open_out pp_file in
  Cil.dumpFile !Cil.printerForMaincil oc "" cil;
  close_out oc

let run work_dir =
  Cil.initCIL ();
  let nodes, _, duedges = initialize work_dir in
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
