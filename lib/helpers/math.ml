(* euclidean mod *)

let ( % ) x y =
  let z = x mod y in
  if z < 0 then z + y else z

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a
