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
    | x when x >= curr_num ->
        (curr_num <> 0 |> Bool.to_int) + ((x - curr_num) / max_num)
    | _ -> 0
  in
  let num_zero_hits = function
    | Right x -> (curr_num + x) / max_num
    | Left x -> num_zero_hits x
  in
  (rotate rotation curr_num, num_zero_hits rotation)

let solve step_fn =
  let rec loop lines curr_num num_zeros =
    match lines with
    | x :: xs ->
        let rot = parse_line x in
        let new_num, hits = step_fn rot curr_num in
        loop xs new_num (num_zeros + hits)
    | [] -> num_zeros
  in
  loop lines 50 0

let sol_1 =
  solve (fun rot curr ->
      let new_curr = rotate rot curr in
      let hits = if new_curr = 0 then 1 else 0 in
      (new_curr, hits))

let sol_0x434C49434B = solve rotate_0x434C49434B
