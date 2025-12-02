(* transform string into list of substrings of size x *)
let rec transform x str =
  if String.length str <= x then
    [str]
  else
    String.sub str 0 x :: transform x (String.sub str x (String.length str - x))

let eq = function
  | [] | [_] -> true
  | x :: xs -> List.for_all ((=) x) xs