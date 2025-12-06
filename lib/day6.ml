let ( << ) = Helpers.Other.( << )
let lines = Helpers.Lines.read_lines "inputs/day6_part1.txt"
let lines2 = Helpers.Lines.read_lines "inputs/day6_part2.txt"
let nums = List.map Helpers.Other.string_to_char_list lines2

let operations =
  Helpers.Lines.read_lines "inputs/day6_operations.txt"
  |> List.hd |> String.split_on_char ','

type operation = { f : int -> int -> int; id : int }

let homework =
  List.map
    (fun line -> List.map int_of_string (String.split_on_char ',' line))
    lines

let string_of_op_to_op = function
  | "+" -> { f = ( + ); id = 0 }
  | "*" -> { f = ( * ); id = 1 }
  | _ -> invalid_arg "can only be * or +"

let rec fold_ops listss = function
  | [] -> []
  | op :: ops ->
      List.fold_left op.f op.id (List.map List.hd listss)
      :: fold_ops (List.map List.tl listss) ops

let column_to_cephalopod_number column =
  let rec column_to_cephalopod_nunber acc = function
    | [] -> acc
    | ' ' :: xs -> column_to_cephalopod_nunber acc xs
    | x :: xs ->
        column_to_cephalopod_nunber ((acc * 10) + Helpers.Other.int_value x) xs
  in
  column_to_cephalopod_nunber 0 column

let is_delimiter = List.for_all (( = ) ' ')

let extract_problem_nums nums =
  let rec extract_problem_nums nums acc =
    let heads = List.map List.hd nums in
    if is_delimiter heads then (acc, List.map List.tl nums)
    else
      extract_problem_nums (List.map List.tl nums)
        (column_to_cephalopod_number heads :: acc)
  in
  extract_problem_nums nums []

let rec find_all_solutions nums = function
  | [] -> []
  | op :: ops ->
      let op_nums, rest_nums = extract_problem_nums nums in
      List.fold_left op.f op.id op_nums :: find_all_solutions rest_nums ops

let sol_1 =
  List.fold_left ( + ) 0
    (fold_ops homework (List.map string_of_op_to_op operations))

let sol_2 =
  List.fold_left ( + ) 0
    (find_all_solutions nums (List.map string_of_op_to_op operations))
