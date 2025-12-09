open Helpers.Graph

let ( % ) = Helpers.Math.( % )
let lines = Helpers.Lines.read_lines "inputs/day9.txt"

type rectangle = Rectangle of node * node

let node_list = List.map node_of_string lines
let global_max_seen = ref 0 

let find_rectange leftmost_node other_nodes =
  let rectangle_area (n1 : node) (n2 : node) =
    match (n1, n2) with
    | { x = x1; y = y1; z = _ }, { x = x2; y = y2; z = _ } ->
        (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
  in
  let rec aux acc = function
    | [] -> acc
    | n :: ns -> aux (rectangle_area leftmost_node n |> max acc) ns
  in
  aux 0 other_nodes

let sol_1 =
  let rec aux acc = function
    | [] -> acc
    | n :: ns -> aux (find_rectange n ns |> max acc) ns
  in
  aux 0 node_list

(* disclaimer. part 2 was really difficult *)
(* In the end, the algorithm that took each rectangle and did *)
(* a walk along the path of nodes to validate if the path intersected *)
(* the rectangle very luckily for me worked. *)

(* HOWEVER. There is an edge case that breaks this. *)
(* Imagine a very very large U shaped polygon with very thin legs. *)
(* Clearly, the path will never intersect the rectangle, but my *)
(* algorithm will happily add the void to the maximum area. *)

(* Luckily, my input contained no such void. BUT... if it did *)
(* Then i would be a bit cooked. *)

(* Todo: prod my friends to see if there is a cleverer trick to use... *)

let intersects_rectangle (Rectangle (n1, n2)) u v =
  (* normalize rectangle bounds *)
  let min_x = min n1.x n2.x
  and max_x = max n1.x n2.x
  and min_y = min n1.y n2.y
  and max_y = max n1.y n2.y in

  let inside p =
    p.x > min_x && p.x < max_x &&
    p.y > min_y && p.y < max_y
  in

  let rec walk p =
    if p = v then false
    else if inside p then true
    else
      let next =
        { p with
            x = p.x + compare v.x p.x;
            y = p.y + compare v.y p.y }
      in
      walk next
  in
  walk u

let rectangle_is_invalid rect =
  let rec aux = function
    | [] | [_] -> false
    | p1 :: (p2 :: _ as rest) ->
        if intersects_rectangle rect p1 p2
        then true
        else aux rest
  in
  aux node_list


let find_rectange_2 leftmost_node other_nodes =
  let area n1 n2 =
    (abs (n1.x - n2.x) + 1) * (abs (n1.y - n2.y) + 1)
  in

  let rec aux = function
    | [] -> !global_max_seen
    | n :: ns ->
        let a = area leftmost_node n in

        if a <= !global_max_seen then
          aux ns
        else
          let rect = Rectangle (leftmost_node, n) in
          if rectangle_is_invalid rect then
            aux ns
          else (
            if a > !global_max_seen then global_max_seen := a;
            aux ns
          )
  in
  aux other_nodes

let sol_2 =
  let rec loop acc = function
    | [] -> acc
    | n :: ns ->
        let best = find_rectange_2 n ns in
        loop (max acc best) ns
  in
  loop 0 node_list
