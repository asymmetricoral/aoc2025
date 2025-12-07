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

let fire_beam_down use_marker origin m =
  let rec aux origin =
    if out_of_bounds origin (Array.length m.(0))
       (* Only block if use_marker is true AND we hit a '|' *)
       || (use_marker && m.(origin.x).(origin.y) = '|')
    then None
    else if m.(origin.x).(origin.y) <> '^' then (
      (* Only mark the path if use_marker is true *)
      if use_marker then m.(origin.x).(origin.y) <- '|';
      aux { origin with x = succ origin.x })
    else Some origin
  in
  aux origin

(* Helper for Part 1: Relies on mutable matrix side-effects *)
let run_part1 m pos_s =
  let rec aux start_pos =
    match fire_beam_down true start_pos m with (* Use marker = true *)
    | None -> 0
    | Some splitter ->
        1
        + aux { splitter with y = pred splitter.y }
        + aux { splitter with y = succ splitter.y }
  in
  aux pos_s

(* Helper for Part 2: Relies on memoization and no matrix side-effects *)
let run_part2 m pos_s =
  let seen = Hashtbl.create 200 in
  let rec aux start_pos =
    match Hashtbl.find_opt seen start_pos with
    | Some x -> x
    | None ->
        begin
          match fire_beam_down false start_pos m with (* Use marker = false *)
          | None ->
              let base = 1 in
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

let num_times_split part =
  let m = fresh_matrix () in
  let pos_s = { x = 0; y = (Array.length m.(0)) / 2 } in
  
  match part with
  | Part1 -> run_part1 m pos_s
  | Part2 -> run_part2 m pos_s

let sol_1 = num_times_split Part1
let sol_2 = num_times_split Part2