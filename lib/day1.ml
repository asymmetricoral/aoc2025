open Helpers.Math

(* types *)
type rotation = Right of int | Left of int

(* constants *)
let max_num = 100
let lines = Helpers.Lines.read_lines "inputs/day1.txt"

(* helpers *)

let parse_line s =
  if String.length s = 0 then invalid_arg "empty string";

  let dir = s.[0] in
  let n = int_of_string (String.sub s 1 (String.length s - 1)) in
  match dir with
  | 'R' -> Right n
  | 'L' -> Left n
  | _ -> invalid_arg "expected R or L"

(* solution *)

let rotate rotation curr_num =
  match rotation with
  | Right x -> (curr_num + x) % max_num
  | Left x -> (curr_num - x) % max_num

let rotate_0x434C49434B rotation curr_num =
  let num_zero_hits = function
    | x when x < curr_num -> 0
    | x when x = curr_num -> 1
    | x when x > curr_num ->
        (if curr_num = 0 then 0 else 1) + ((x - curr_num) / max_num)
    | _ -> 1
  in
  let new_num = rotate rotation curr_num in
  match rotation with
  | Right x -> (new_num, (curr_num + x) / max_num)
  | Left x -> (new_num, num_zero_hits x)

let sol_1 =
  let rec solve lines curr_num num_zeros =
    match lines with
    | x :: xs -> (
        match rotate (parse_line x) curr_num with
        | 0 -> solve xs 0 (num_zeros + 1)
        | x -> solve xs x num_zeros)
    | [] -> num_zeros
  in
  solve lines 50 0

let sol_0x434C49434B =
  let rec solve lines curr_num num_zeros =
    match lines with
    | x :: xs ->
        let new_num, num_zero_hits =
          rotate_0x434C49434B (parse_line x) curr_num
        in
        solve xs new_num (num_zeros + num_zero_hits)
    | [] -> num_zeros
  in
  solve lines 50 0
