open Helpers.Other

(* constants *)
let lines = Helpers.Lines.read_lines "inputs/day3.txt"

(* parsers *)

(* takes a bank and converts it to an int list *)
let parse_bank line =
  List.init (String.length line) (int_value << String.get line)

let rec build_maximal_list = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs ->
      let maximal_list = build_maximal_list xs in
      Int.max x (List.hd maximal_list) :: maximal_list

(* does the pairing of the prefixes with the suffices *)
(* the rank is the tens unit, i'll come up with a better term later *)
let find_joltages bank suffices rank =
  let rec find_joltages bank suffices =
    match (bank, suffices) with
    | _, [] -> []
    | x :: xs, y :: ys -> ((x * Helpers.Math.pow 10 rank) + y) :: find_joltages xs ys
    | _ -> invalid_arg "impossible configuration"
  in
  find_joltages bank suffices

let build_joltages bank =
  let step_fn maximal_list rank =
    find_joltages bank (List.tl maximal_list) rank |> build_maximal_list
  in
  let rec build_joltages maximal_list rank =
    if rank = 12 then maximal_list
    else build_joltages (step_fn maximal_list rank) (rank + 1)
  in
  build_joltages (build_maximal_list bank) 1

let joltage bank =
  let rec joltage bank maximal_list =
    match (bank, maximal_list) with
    | [ _ ], [] -> -1
    | x :: xs, y :: ys -> Int.max ((x * 10) + y) @@ joltage xs ys
    | _ -> invalid_arg "impossible configuration"
  in
  joltage bank (List.tl @@ build_maximal_list bank)

let sol_1 = List.fold_left ( + ) 0 (List.map (joltage << parse_bank) lines)
let sol_2 = List.fold_left ( + ) 0 (List.map (List.hd << build_joltages << parse_bank) lines)