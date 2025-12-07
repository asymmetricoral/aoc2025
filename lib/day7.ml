(* chop off the last line to create a square matrix *)
let lines = Helpers.Lines.read_lines "inputs/day7.txt"

type part = Part1 | Part2

(* Always return a fresh matrix for each part *)
let fresh_matrix () =
  Helpers.Mymatrix.create_square_matrix lines

(* Top right at (0, sq_mat.(0).length - 1) *)
type coordinates = { x : int; y : int }

let out_of_bounds coords arr_length =
  coords.x < 0 || coords.y < 0 || coords.x >= arr_length
  || coords.y >= arr_length

let fire_beam_down part origin m =
  let rec aux origin =
    if out_of_bounds origin (Array.length m.(0))
       ||
       (match part with
        | Part1 -> m.(origin.x).(origin.y) = '|'
        | Part2 -> false)
    then None
    else if m.(origin.x).(origin.y) <> '^' then (
      m.(origin.x).(origin.y) <- '|';
      aux { origin with x = succ origin.x })
    else Some origin
  in
  aux origin

let num_times_split part =
  let m = fresh_matrix () in
  let seen = Hashtbl.create 200 in
  let pos_s = { x = 0; y = (Array.length m.(0)) / 2 } in

  let rec aux start_pos =
    match Hashtbl.find_opt seen start_pos with
    | Some x -> x
    | None ->
        begin
          match fire_beam_down part start_pos m with
          | None ->
              let base = match part with Part1 -> 0 | Part2 -> 1 in
              Hashtbl.add seen start_pos base;
              base
          | Some splitter ->
              let left = aux { splitter with y = pred splitter.y } in
              let right = aux { splitter with y = succ splitter.y } in
              let total = left + right in
              Hashtbl.add seen start_pos total;
              total
        end
  in
  aux pos_s

let sol_1 = num_times_split Part1
let sol_2 = num_times_split Part2