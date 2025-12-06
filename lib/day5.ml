let ingredients_lines = Helpers.Lines.read_lines "inputs/day5_ingredients.txt"
let ranges_lines = Helpers.Lines.read_lines "inputs/day5_ranges.txt"

(* types *)

type range = Range of int * int

let range_of_string s =
  match String.split_on_char '-' s with
  | [ a; b ] -> Range (int_of_string a, int_of_string b)
  | _ -> invalid_arg "range_of_string: expected format \"a-b\""

let compare_range (Range (x1, y1)) (Range (x2, y2)) =
  let c = compare x1 x2 in
  if c <> 0 then c else compare y1 y2

(* parsing *)
let ingredients = List.map int_of_string ingredients_lines |> List.sort compare
let ranges = List.map range_of_string ranges_lines |> List.sort compare_range
let is_n_in_range n (Range (min, max)) = min <= n && n <= max
let is_n_before_range n (Range (min, _)) = n < min

let rec count_fresh_ingredients sorted_ranges sorted_ingredients =
  match (sorted_ranges, sorted_ingredients) with
  | [], _ | _, [] -> 0
  | r :: rs, y :: ys ->
      if is_n_before_range y r then count_fresh_ingredients (r :: rs) ys
      else if is_n_in_range y r then 1 + count_fresh_ingredients (r :: rs) ys
      else count_fresh_ingredients rs (y :: ys)

let merge_ranges (Range (x1, y1)) (Range (_, y2)) = Range (x1, max y1 y2)

let rec find_maximal_ranges = function
  | [] -> []
  | [ x ] -> [ x ]
  | Range (x1, y1) :: Range (x2, y2) :: rs when y1 < x2 ->
      Range (x1, y1) :: find_maximal_ranges (Range (x2, y2) :: rs)
  | r1 :: r2 :: rs -> find_maximal_ranges (merge_ranges r1 r2 :: rs)

let sol_1 = count_fresh_ingredients ranges ingredients

let sol_2 =
  List.fold_left
    (fun acc (Range (x, y)) -> acc + (y - x + 1))
    0
    (find_maximal_ranges ranges)
