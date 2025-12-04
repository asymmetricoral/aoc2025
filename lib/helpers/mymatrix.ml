let (<<) = Other.(<<)

let print_matrix m =
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length m.(i) - 1 do
      print_char m.(i).(j);
      print_string " "
    done;
    print_endline ""
  done

let create_square_matrix lines =
  let m = List.map (Array.of_list << Other.string_to_char_list) lines in
  Array.of_list m
