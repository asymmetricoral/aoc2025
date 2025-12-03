let ( << ) = Helpers.Other.( << )
let int_value = Helpers.Other.int_value
let build_maximal_list = Helpers.Mylist.build_maximal_list

(* constants *)
let lines = Helpers.Lines.read_lines "inputs/day3.txt"

(* parsers *)

(* takes a bank and converts it to an int list *)
let parse_bank line =
  List.init (String.length line) (int_value << String.get line)

(* does the pairing of the prefixes with the suffices *)
let find_joltages bank suffices exponent =
  let rec find_joltages bank suffices =
    match (bank, suffices) with
    | _, [] -> []
    | x :: xs, y :: ys ->
        ((x * Helpers.Math.pow 10 exponent) + y) :: find_joltages xs ys
    | _ -> invalid_arg "impossible configuration"
  in
  find_joltages bank suffices

let build_joltages bank =
  let step_fn maximal_list exponent =
    find_joltages bank (List.tl maximal_list) exponent |> build_maximal_list
  in
  let rec build_joltages maximal_list exponent =
    if exponent = 12 then maximal_list
    else build_joltages (step_fn maximal_list exponent) (exponent + 1)
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

let sol_2 =
  List.fold_left ( + ) 0
    (List.map (List.hd << build_joltages << parse_bank) lines)
