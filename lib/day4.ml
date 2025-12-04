open Helpers

let ( << ) = Other.( << )
let lines = Helpers.Lines.read_lines "inputs/day4.txt"
let layout = Mymatrix.create_square_matrix lines

let count_adjacent_rolls m x y =
  let square_mat_size = Array.length m in
  let should_skip (refx, refy) (x, y) =
    x < 0 || x >= square_mat_size || y < 0 || y >= square_mat_size
    || (refx = x && refy = y)
  in
  let is_roll i j = m.(i).(j) = '@' in
  let count_rolls = ref 0 in
  for i = x - 1 to x + 1 do
    for j = y - 1 to y + 1 do
      if should_skip (x, y) (i, j) then ()
      else count_rolls := !count_rolls + Bool.to_int (is_roll i j)
    done
  done;
  !count_rolls

let should_remove layout i j =
  layout.(i).(j) = '@' && count_adjacent_rolls layout i j < 4

let sol_1 =
  let count = ref 0 in
  for i = 0 to Array.length layout - 1 do
    for j = 0 to Array.length layout - 1 do
      if should_remove layout i j then incr count
    done
  done;
  !count

let remove_rolls () =
  let count = ref 0 in
  for i = 0 to Array.length layout - 1 do
    for j = 0 to Array.length layout - 1 do
      if should_remove layout i j then (
        layout.(i).(j) <- '.';
        incr count)
    done
  done;
  !count

let rec sol_2 () =
  let n = remove_rolls () in
  if n = 0 then 0 else n + sol_2 ()
