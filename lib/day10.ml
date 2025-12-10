let ( << ) = Helpers.Other.( << )
let lines = Helpers.Lines.read_lines "inputs/day10.txt"

type button = int
type indicator_lights = { mask : int; num_lights : int }

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type machine = {
  indicator_lights : indicator_lights;
  buttons : button list;
  joltages : int list;
}

(*
  takes a string like (0, 3),
  and assuming length_lights > max_int
  then bit 0 and 3 are 1s and the rest are 0s
*)
let string_to_button str length_lights =
  (* remove brackets *)
  let s = String.sub str 1 (String.length str - 2) in

  let indices = String.split_on_char ',' s in
  List.fold_left
    (fun acc index_str ->
      try
        let index = int_of_string (String.trim index_str) in
        if index >= 0 && index < length_lights then acc lor (1 lsl index)
        else acc
      with Failure _ -> acc)
    0 indices

(* takes a string like [.##.] and converts it to a binary number like 0110 *)
let string_to_lights str =
  let s = String.sub str 1 (String.length str - 2) in

  let rec loop acc index =
    if index >= String.length s then acc
    else
      let bit =
        match s.[index] with
        | '#' -> 1
        | '.' -> 0
        | _ -> invalid_arg "must only have # or ."
      in

      let bit_position = index in

      loop (acc lor (bit lsl bit_position)) (index + 1)
  in
  loop 0 0

let string_to_machine str =
  let splitted_line = String.split_on_char ' ' str in
  let lights_str = List.hd splitted_line in

  let lights_mask = string_to_lights lights_str in
  let num_lights = String.length lights_str - 2 in

  let indicator_lights = { mask = lights_mask; num_lights } in

  (* remove parentheses *)
  let joltages_str = List.hd (List.rev splitted_line) in
  let joltages_s = String.sub joltages_str 1 (String.length joltages_str - 2) in
  (* remove braces *)
  let joltages =
    String.split_on_char ',' joltages_s
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
  in

  let button_strs = List.tl splitted_line |> List.rev |> List.tl |> List.rev in
  let buttons = List.map (fun s -> string_to_button s num_lights) button_strs in

  { indicator_lights; buttons; joltages }

(* parsing done, time for solving *)
let is_all_off lights_state = lights_state.mask = 0

let flip_lights lights_state button =
  let new_mask = lights_state.mask lxor button in

  { lights_state with mask = new_mask }

let find_fewest_button_presses machine =
  let q = Queue.create () in
  (* sets of indicator_lights *)
  (* if we've already seen the configuration *)
  (* then we're stuck in a loop. *)
  (* we will not add these to the queue. *)
  let cache = IntSet.empty in

  let add lights num_pressed cache =
    let current_mask = lights.mask in

    if IntSet.mem current_mask cache then cache
    else
      let new_cache = IntSet.add current_mask cache in

      List.iter
        (fun b ->
          let item = (b, lights, num_pressed) in
          Queue.add item q)
        machine.buttons;

      new_cache
  in

  let initial_cache = add machine.indicator_lights 0 cache in

  let rec loop q current_cache =
    if Queue.is_empty q then failwith "impossible configuration"
    else
      let button_to_press, lights_before, num_pressed = Queue.pop q in

      (* we perform the next configuration's operation *)
      let new_lights = flip_lights lights_before button_to_press in

      if is_all_off new_lights then num_pressed + 1
      else
        (* then we add this new light configuration and all other buttons to press *)
        let updated_cache = add new_lights (num_pressed + 1) current_cache in

        (* then we loop *)
        loop q updated_cache
  in
  loop q initial_cache

let sol_1 =
  let process_line_to_result line =
    line |> string_to_machine |> find_fewest_button_presses
  in
  List.fold_left ( + ) 0 (List.map process_line_to_result lines)
