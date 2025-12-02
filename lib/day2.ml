open Helpers.Math
open Helpers.Mylist

(* constants *)
let lines = Helpers.Lines.read_lines "inputs/day2.txt"

(* helpers *)

let parse_input line =
  let parse_range segment =
    match String.split_on_char '-' segment with
    | [ a; b ] -> (int_of_string a, int_of_string b)
    | _ -> invalid_arg "invalid segment"
  in
  String.split_on_char ',' line |> List.map parse_range

(* brute force *)

let is_invalid_id x substr_length =
  let int_as_str = string_of_int x in
  let str_length = String.length int_as_str in
  str_length % substr_length = 0 && (eq @@ transform substr_length int_as_str)

let is_invalid_id_pt1 x =
  let string_length = String.length @@ string_of_int x in
  string_length % 2 = 0 && is_invalid_id x (string_length / 2)

let is_invalid_id_pt2 x =
  let max_substr_length = (String.length @@ string_of_int x) / 2 in
  List.init max_substr_length (fun x -> x + 1)
  |> List.map (is_invalid_id x)
  |> List.fold_left ( || ) false

let rec invalid_ids start_range end_range =
  match start_range with
  | hd when start_range <= end_range && is_invalid_id_pt1 hd ->
      hd :: invalid_ids (start_range + 1) end_range
  | _ when start_range > end_range -> []
  | _ -> invalid_ids (start_range + 1) end_range

let sum_invalid_ids (start_range, end_range) =
  invalid_ids start_range end_range |> List.fold_left ( + ) 0

let sol_1 =
  List.fold_left ( + ) 0
  @@ List.map sum_invalid_ids (List.hd lines |> parse_input)
