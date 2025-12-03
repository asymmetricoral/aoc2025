(* infix composition *)
let (<<) f g x = f(g(x));;

let int_value c = int_of_char c - int_of_char '0'