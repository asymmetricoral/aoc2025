(* transform string into list of substrings of size x *)
let rec transform x str =
  if String.length str <= x then
    [str]
  else
    String.sub str 0 x :: transform x (String.sub str x (String.length str - x))

let eq = function
  | [] | [_] -> true
  | x :: xs -> List.for_all ((=) x) xs

(* builds the maximum suffix list *)
let rec build_maximal_list = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs ->
      let maximal_list = build_maximal_list xs in
      Int.max x (List.hd maximal_list) :: maximal_list