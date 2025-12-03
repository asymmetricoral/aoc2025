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

let joltage bank =
  let rec joltage bank maximal_list = 
    match bank, maximal_list with
    | [_], [] -> -1
    | x::xs, y::ys -> Int.max (x * 10 + y) @@ joltage xs ys
    | _ -> invalid_arg "impossible configuration"
in joltage bank (List.tl @@ build_maximal_list bank)

let sol_1 = List.fold_left ( + ) 0 (List.map (joltage << parse_bank) lines)
