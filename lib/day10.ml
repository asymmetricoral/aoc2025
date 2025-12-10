let ( << ) = Helpers.Other.( << )
let lines = Helpers.Lines.read_lines "inputs/day10.txt"

type button = { bits : int; vector : int array }
type indicator_lights = { mask : int; num_lights : int }
type joltage = int

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type machine = {
  indicator_lights : indicator_lights;
  buttons : button list;
  joltages : joltage array;
}

let string_to_button str length_lights =
  let bit_vector = Array.make length_lights 0 in
  let s = String.sub str 1 (String.length str - 2) in
  let indices = String.split_on_char ',' s in

  let final_mask =
    List.fold_left
      (fun mask index_str ->
        try
          let index = int_of_string (String.trim index_str) in
          if index >= 0 && index < length_lights then (
            bit_vector.(index) <- 1;
            mask lor (1 lsl index) (* Calculate the bitmask *))
          else mask
        with Failure _ -> mask)
      0 indices
  in
  { bits = final_mask; vector = bit_vector }

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

  let joltages_str = List.hd (List.rev splitted_line) in
  let joltages_s = String.sub joltages_str 1 (String.length joltages_str - 2) in
  let joltages =
    String.split_on_char ',' joltages_s
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string |> Array.of_list
  in

  let button_strs = List.tl splitted_line |> List.rev |> List.tl |> List.rev in
  let buttons = List.map (fun s -> string_to_button s num_lights) button_strs in

  { indicator_lights; buttons; joltages }

(* Utility functions *)
let is_all_off lights_state = lights_state.mask = 0

let is_all_zero (joltages_state : joltage array) =
  Array.for_all (fun j -> j = 0) joltages_state

let flip_lights lights_state button =
  let new_mask = lights_state.mask lxor button.bits in
  { lights_state with mask = new_mask }

let decrement_joltages joltages button = Array.map2 ( - ) joltages button.vector

(* --- CORE GENERIC SOLVER --- *)

let solve_puzzle_generic (initial_state : 'a) (buttons : button list)
    (get_cache_key : 'a -> int) (is_goal : 'a -> bool)
    (transition : 'a -> button -> 'a option) =
  let q = Queue.create () in
  let cache = IntSet.empty in

  let add state num_pressed cache =
    let current_key = get_cache_key state in

    if IntSet.mem current_key cache then cache
    else
      let new_cache = IntSet.add current_key cache in

      List.iter
        (fun b ->
          let item = (b, state, num_pressed) in
          Queue.add item q)
        buttons;

      new_cache
  in

  let initial_cache = add initial_state 0 cache in

  let rec loop q current_cache =
    if Queue.is_empty q then failwith "impossible configuration"
    else
      let button_to_press, state_before, num_pressed = Queue.pop q in

      match transition state_before button_to_press with
      | Some new_state ->
          if is_goal new_state then num_pressed + 1
          else
            let updated_cache = add new_state (num_pressed + 1) current_cache in
            loop q updated_cache
      | None -> loop q current_cache
  in
  loop q initial_cache

(* --- PART 1 SPECIFIC IMPLEMENTATION --- *)

let find_fewest_button_presses machine =
  let lights_transition lights button =
    let new_lights = flip_lights lights button in
    Some new_lights
  in

  solve_puzzle_generic machine.indicator_lights machine.buttons
    (fun lights -> lights.mask) (* get_cache_key: lights.mask *)
    is_all_off lights_transition

(* --- PART 2 SPECIFIC IMPLEMENTATION --- *)

let solve_joltage_puzzle machine =
  let joltage_array_to_int joltage_arr =
    Array.fold_left (fun acc j -> (acc lsl 8) + j) 0 joltage_arr
  in

  let joltage_transition joltages button =
    let new_joltages = decrement_joltages joltages button in

    if Array.for_all (fun j -> j >= 0) new_joltages then Some new_joltages
    else None
  in

  solve_puzzle_generic machine.joltages machine.buttons
    joltage_array_to_int (* get_cache_key: joltage array hash *)
    is_all_zero joltage_transition

(* --- MAIN CALLS --- *)

let sol_1 =
  let process_line_to_result line =
    line |> string_to_machine |> find_fewest_button_presses
  in
  List.fold_left ( + ) 0 (List.map process_line_to_result lines)

let sol_2 =
  let process_line_to_result line =
    Printf.printf "Processing: %s\n" line;
    flush stdout;
    line |> string_to_machine |> solve_joltage_puzzle
  in
  List.fold_left ( + ) 0 (List.map process_line_to_result lines)
