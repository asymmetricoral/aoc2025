(* chop off the last line to create a square matrix *)
let lines = Helpers.Lines.read_lines "inputs/day7.txt"
let sq_mat = Helpers.Mymatrix.create_square_matrix lines

(* Top right at (0, sq_mat.(0).length - 1) *)
type coordinates = { x : int; y : int }

let pos_s = { x = 0; y = (sq_mat.(0) |> Array.length) / 2 }

let out_of_bounds coords arr_length =
  coords.x < 0 || coords.y < 0 || coords.x >= arr_length
  || coords.y >= arr_length

let fire_beam_down origin m =
  let rec fire_beam_down origin =
    if
      out_of_bounds origin (Array.length m.(0))
      || m.(origin.x).(origin.y) == '|'
    then None
    else if m.(origin.x).(origin.y) <> '^' then (
      m.(origin.x).(origin.y) <- '|';
      fire_beam_down { origin with x = succ origin.x })
    else Some origin
  in
  fire_beam_down origin

let num_times_split m =
  let rec num_times_split start_pos =
    match fire_beam_down start_pos m with
    | None -> 0
    | Some splitter ->
        1
        + num_times_split { splitter with y = pred splitter.y }
        + num_times_split { splitter with y = succ splitter.y }
  in
  num_times_split pos_s

let sol_1 = num_times_split sq_mat
