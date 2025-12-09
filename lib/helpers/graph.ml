type node = { x : int; y : int; z : int }

let compare_node a b =
  match Int.compare a.x b.x with
  | 0 -> ( match Int.compare a.y b.y with 0 -> Int.compare a.z b.z | c -> c)
  | c -> c

module NodeSet = Set.Make (struct
  type t = node

  let compare = Stdlib.compare
end)

let node_of_string line =
  let i = int_of_string in
  match String.split_on_char ',' line with
  | [ a; b; c ] -> { x = i a; y = i b; z = i c }
  | [ a; b ] -> { x = i a; y = i b; z = 0 }
  | _ -> invalid_arg "invalid configuration"

let print_node { x; y; z } =
  Printf.printf "{ x = %d; y = %d; z = %d }" x y z

let print_nodeset set =
  Printf.printf "NodeSet = {\n";
  NodeSet.iter (fun n ->
    Printf.printf "  ";
    print_node n;
    Printf.printf "\n"
  ) set;
  Printf.printf "}\n%!";
