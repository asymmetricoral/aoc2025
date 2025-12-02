open Helpers.Math

(* constants *)
let lines = Helpers.Lines.read_lines "inputs/day2.txt"

(* brute force *)

let is_unique_id x =
  let int_as_str = string_of_int x in
  let str_length = String.length int_as_str in
  let halfway = str_length / 2 in
  str_length % 2 = 0
  && String.sub int_as_str 0 halfway
     = String.sub int_as_str halfway halfway

let rec unique_ids start_range end_range =
  match start_range with
  | hd when start_range <> end_range && is_unique_id hd ->
      hd :: unique_ids (start_range + 1) end_range
  | _ -> unique_ids (start_range + 1) end_range

let sum_unique_ids (start_range, end_range) =
  unique_ids start_range end_range |> List.fold_left ( + ) 0

let parse_input line =
  let parse_range segment =
    match String.split_on_char '-' segment with
      | [a; b] -> (int_of_string a, int_of_string b)
      | _ -> invalid_arg "invalid segment"
  in
  String.split_on_char ',' line |> List.map parse_range

let sol_1 =
  List.fold_left (+) 0 @@ List.map sum_unique_ids (List.hd lines |> parse_input)