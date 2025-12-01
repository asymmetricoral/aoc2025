(* euclidean mod *)

let (%) x y =
  let z = x mod y in
  if z < 0 then z + y else z