(* infix composition *)
let ( << ) f g x = f (g x)
let int_value c = int_of_char c - int_of_char '0'

let string_to_char_list s = List.init (String.length s) (String.get s)