let lines = Helpers.Lines.read_lines "inputs/day8.txt"
let num_connections = 1000

type node = { x : float; y : float; z : float }

module NodeSet = Set.Make (struct
  type t = node

  let compare = Stdlib.compare
end)

let print_node (n : node) =
  Printf.printf "Node(x = %f, y = %f, z = %f)\n" n.x n.y n.z

let print_nodeset s =
  Printf.printf "{ ";
  NodeSet.iter
    (fun n ->
      print_node n;
      Printf.printf " ")
    s;
  Printf.printf "}\n"

type distance = { dist : float; u : node; v : node }

let compare_dist d1 d2 = compare d1.dist d2.dist

let node_of_string line =
  let i = float_of_string in
  match String.split_on_char ',' line with
  | [ a; b; c ] -> { x = i a; y = i b; z = i c }
  | _ -> invalid_arg "invalid configuration"

let dist u v =
  let dx = v.x -. u.x in
  let dy = v.y -. u.y in
  let dz = v.z -. u.z in
  Float.sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz))

let dist_of_nodes u v = { dist = dist u v; u; v }

let distances lines =
  let rec distances nodes acc =
    match nodes with
    | [] -> acc
    | x :: xs -> distances xs @@ List.map (dist_of_nodes x) xs @ acc
  in
  distances (List.map node_of_string lines) []

let rec add_node_to_circuit circuits old_node new_node =
  match circuits with
  | [] -> [ NodeSet.empty |> NodeSet.add old_node |> NodeSet.add new_node ]
  | c :: cs -> (
      match NodeSet.mem old_node c || NodeSet.mem new_node c with
      | true -> (NodeSet.add new_node c |> NodeSet.add old_node) :: cs
      | false -> c :: add_node_to_circuit cs old_node new_node)

let merge_circuits old_node new_node circuits =
  let merge_circuits sets = List.fold_left NodeSet.union NodeSet.empty sets in
  let rec aux circuits_to_merge acc = function
    | [] -> merge_circuits circuits_to_merge :: acc
    | c :: cs -> (
        match NodeSet.mem old_node c || NodeSet.mem new_node c with
        | true -> aux (c :: circuits_to_merge) acc cs
        | false -> aux circuits_to_merge (c :: acc) cs)
  in
  aux [] [] circuits

let sol_1 =
  let distance_list =
    Helpers.Mylist.take num_connections
    @@ List.sort compare_dist (distances lines)
  in
  let rec find_circuits distance_list circuits =
    match distance_list with
    | [] ->
        List.sort
          (fun s1 s2 -> compare (NodeSet.cardinal s1) (NodeSet.cardinal s2))
          circuits
    | d :: ds ->
        find_circuits ds
          (add_node_to_circuit circuits d.u d.v |> merge_circuits d.u d.v)
  in
  let circuits_full = find_circuits distance_list [] in
  let first_three = Helpers.Mylist.take 3 (List.rev circuits_full) in
  List.fold_left ( * ) 1 (List.map NodeSet.cardinal first_three)

let sol_2 =
  let distance_list = List.sort compare_dist (distances lines) in
  let rec find_circuits distance_list circuits =
    match distance_list with
    | [] -> failwith "this condition should never be hit."
    | d :: ds -> (
        let new_circuit =
          add_node_to_circuit circuits d.u d.v |> merge_circuits d.u d.v
        in
        match new_circuit with
        | [ x ] when NodeSet.cardinal x = List.length lines -> d.u.x *. d.v.x
        | _ -> find_circuits ds new_circuit)
  in
  find_circuits distance_list []
