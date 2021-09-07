module Node = struct
  include String

  let hash = Hashtbl.hash
end

module Edge = struct
  type t = string

  let compare = compare

  let default = ""
end

module G = struct
  include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  let default_vertex_attributes _ = []

  let default_edge_attributes _ = []

  let edge_attributes (_, edge, _) =
    if edge = "n1" then [ `Color 0xff0000 ] else [ `Color 0x000000 ]

  let get_subgraph _ = None

  let vertex_attributes _ = [ `Shape `Box ]

  let vertex_name v = "\"" ^ v ^ "\""

  let graph_attributes _ = []
end

module Graphviz = Graph.Graphviz.Dot (G)

let rec draw ic name pred graph =
  match input_line ic with
  | s -> G.add_edge_e graph (pred, name, s) |> draw ic name s
  | exception _ -> graph

let main () =
  Array.to_list Sys.argv |> List.tl
  |> List.fold_left
       (fun graph file ->
         let ic = open_in file in
         let graph = draw ic file "__START__" graph in
         close_in ic;
         graph)
       G.empty
  |> Graphviz.output_graph stdout

let _ = main ()
