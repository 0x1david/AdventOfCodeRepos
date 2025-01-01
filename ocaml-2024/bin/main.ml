module DayOne = struct
  [@@@warning "-32"]

  let is_digit = function '0' .. '9' -> true | _ -> false

  let read_lines filename =
    let ichan = open_in filename in
    let try_read () = try Some (input_line ichan) with End_of_file -> None in
    let rec loop acc =
      match try_read () with
      | Some value -> loop @@ (value :: acc)
      | None ->
          close_in ichan;
          let result = List.rev acc in
          result
    in
    loop []

  let chars_to_int chars =
    let result =
      chars |> List.map Char.escaped |> String.concat "" |> int_of_string
    in
    result

  let separate_line acc line =
    let len = String.length line in
    let rec process_chars pos current_num =
      if pos >= len then
        match current_num with
        | [] -> acc
        | nums ->
            let result = acc @ [ chars_to_int (List.rev nums) ] in
            result
      else
        let c = line.[pos] in
        if is_digit c then process_chars (pos + 1) (c :: current_num)
        else if List.is_empty current_num then process_chars (pos + 1) []
        else
          process_chars (pos + 1) [] @ [ chars_to_int (List.rev current_num) ]
    in
    let result = process_chars 0 [] in
    result

  let rec separate_lines acc = function
    | [] -> acc
    | line :: rest ->
        let numbers = separate_line acc line in
        separate_lines numbers rest

  let rec create_pairs acc = function
    | [] -> acc
    | [ _ ] -> acc
    | first :: second :: rest ->
        let new_acc = (second, first) :: acc in
        create_pairs new_acc rest

  let rec fst_list acc = function
    | [] -> List.rev acc
    | (left, _) :: rest -> fst_list (left :: acc) rest

  let rec snd_list acc = function
    | [] -> List.rev acc
    | (_, right) :: rest -> snd_list (right :: acc) rest

  let rec count_distance sum = function
    | [] -> sum
    | (left, right) :: rest -> count_distance (abs (left - right) + sum) rest

  let count_similarity_score left rightlist =
    let cnt = List.length @@ List.filter (( = ) left) rightlist in
    cnt * left

  let rec count_all_sim_scores rightlist = function
    | [] -> 0
    | h :: t ->
        count_similarity_score h rightlist + count_all_sim_scores rightlist t
end

module DayTwo = struct
  [@@@warning "-32"]

  let file = "bin/input/input_two"
  let test_file = "bin/test/test_two"
  let split_on_space = String.split_on_char ' '
  let split_on_space_list lines = List.map split_on_space lines

  let _bounded_asc curr last =
    let diff = curr - last in
    diff < 4

  let rec _asc last = function
    | [] -> true
    | h :: t -> if h > last && _bounded_asc h last then _asc h t else false

  let asc = function [] -> true | h :: t -> _asc h t

  let _bounded_desc curr last =
    let diff = last - curr in
    diff < 4

  let rec _desc last = function
    | [] -> true
    | h :: t -> if h < last && _bounded_desc h last then _desc h t else false

  let desc = function [] -> true | h :: t -> _desc h t
  let create_mut idx lst = List.filteri (fun i _ -> i <> idx) lst

  let rec create_all_muts idx up_bound acc lst =
    if idx >= up_bound then acc
    else create_all_muts (idx + 1) up_bound (create_mut idx lst :: acc) lst

  let is_safe x = asc x || desc x
  let not_safe x = not (asc x || desc x)

  let get_result () =
    let codes =
      file |> DayOne.read_lines |> split_on_space_list
      |> List.map (List.map int_of_string)
    in

    let safe = codes |> List.filter is_safe in
    let unsafe = codes |> List.filter not_safe in
    let unsafe_muts =
      List.map (fun lst -> create_all_muts 0 (List.length lst) [] lst) unsafe
    in
    let new_safe = List.filter (List.exists is_safe) unsafe_muts in
    List.length safe + List.length new_safe
end

module DayThree = struct
  [@@@warning "-32"]

  let file = "bin/input/input_three"
  let test_file = "bin/test/test_three"
  let pat = Str.regexp "mul(\\([1-9][0-9]*\\),\\([1-9][0-9]*\\))"

  let split_and_collect s =
    let do_pat = Str.regexp "do()" in
    let dont_pat = Str.regexp "don't()" in
    let split_and_cut str = List.nth (Str.split dont_pat str) 0 in
    let splits = List.map split_and_cut @@ Str.split do_pat s in
    List.fold_left ( ^ ) "" splits

  let find_uncorrupted pat haystack =
    let clean_haystack = split_and_collect haystack in
    let rec find_all pos acc =
      try
        let pos = Str.search_forward pat clean_haystack pos in
        let match1 = int_of_string @@ Str.matched_group 1 clean_haystack in
        let match2 = int_of_string @@ Str.matched_group 2 clean_haystack in
        find_all (pos + 1) ((match1, match2) :: acc)
      with Not_found -> List.rev acc
    in

    find_all 0 []

  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

  let mul_tuple = function fst, snd -> fst * snd

  let _get_result fp =
    read_whole_file fp |> find_uncorrupted pat |> List.map mul_tuple
    |> List.fold_left ( + ) 0

  let get_test_result () = _get_result test_file
  let get_result () = _get_result file
end

module DayFour = struct
  [@@@warning "-32"]

  type word_result = { start_pos : int * int; direction : string }

  let file = "bin/input/input_four"
  let test_file = "bin/test/test_four"
  let word_to_find = "MAS"

  type direction_fn = int -> int -> int * int

  module Direction = struct
    type t = direction_fn

    let up_left x y = (x - 1, y - 1)
    let up_right x y = (x + 1, y - 1)
    let down_left x y = (x - 1, y + 1)
    let down_right x y = (x + 1, y + 1)

    let to_string (dir : t) : string =
      match dir with
      | d when d == up_left -> "up_left"
      | d when d == up_right -> "up_right"
      | d when d == down_left -> "down_left"
      | d when d == down_right -> "down_right"
      | _ -> failwith "Unknown direction"

    let of_string (s : string) : t =
      match s with
      | "up_left" -> up_left
      | "up_right" -> up_right
      | "down_left" -> down_left
      | "down_right" -> down_right
      | _ -> failwith "Invalid direction string"

    let opposite (dir : string) : t * t =
      match dir with
      | "up_left" -> (up_right, down_left)
      | "up_right" -> (up_left, down_right)
      | "down_left" -> (down_right, up_left)
      | "down_right" -> (down_left, up_right)
      | _ -> failwith "Invalid direction string"
  end

  let a_of_result res =
    let x, y = res.start_pos in
    Direction.of_string res.direction x y

  let cross_of_result res crs =
    let x_of_a, y_of_a = a_of_result res in
    let one_fn, two_fn = Direction.opposite res.direction in
    let x_of_alt_one, y_of_alt_one = one_fn x_of_a y_of_a in
    let x_of_alt_two, y_of_alt_two = two_fn x_of_a y_of_a in

    let get_char x y = String.get (List.nth crs y) x in

    match
      (get_char x_of_alt_one y_of_alt_one, get_char x_of_alt_two y_of_alt_two)
    with
    | 'M', 'S' ->
        Some
          {
            start_pos = (x_of_alt_one, y_of_alt_one);
            direction = Direction.to_string one_fn;
          }
    | 'S', 'M' ->
        Some
          {
            start_pos = (x_of_alt_two, y_of_alt_two);
            direction = Direction.to_string two_fn;
          }
    | _ -> None

  let check_letter x y crs letter =
    Printf.printf "Checking letter at position (%d, %d): expecting '%c'\n" x y
      letter;
    let col = List.nth crs y in
    let found = String.get col x in
    Printf.printf "Found letter: '%c'\n" found;
    found = letter

  let rec walk_check f x y crs letter_pos =
    Printf.printf "Walking check at position (%d, %d), letter position: %d\n" x
      y letter_pos;
    if letter_pos >= String.length word_to_find then true
    else
      let new_x, new_y = f x y in
      let searched_letter = String.get word_to_find letter_pos in
      let found = check_letter new_x new_y crs searched_letter in
      Printf.printf "Letter check result: %b\n" found;
      if found then walk_check f new_x new_y crs @@ (letter_pos + 1) else false

  let find_word h w x y crs =
    Printf.printf "\nSearching for word at starting position (%d, %d)\n" x y;
    let word_len = String.length word_to_find in
    let directions =
      [
        (Direction.up_left, x >= word_len - 1 && y >= word_len - 1, "up_left");
        (Direction.up_right, w - x >= word_len && y >= word_len - 1, "up_right");
        ( Direction.down_left,
          x >= word_len - 1 && h - y >= word_len,
          "down_left" );
        ( Direction.down_right,
          w - x >= word_len && h - y >= word_len,
          "down_right" );
      ]
    in
    List.fold_left
      (fun acc (f, cond, dir_name) ->
        Printf.printf "Checking direction: %s, condition: %b\n" dir_name cond;
        if cond && walk_check f x y crs 1 then (
          let result = { start_pos = (x, y); direction = dir_name } in
          Printf.printf "Found word in direction %s!\n" dir_name;
          match cross_of_result result crs with
          | Some c -> (result, c) :: acc
          | None -> acc)
        else acc)
      [] directions

  let rec walk_crossword h w x y crs acc =
    Printf.printf "\nWalking crossword at (%d, %d), current results: %d\n" x y
      (List.length acc);
    let col = List.nth crs y in
    let found = String.get col x in
    Printf.printf "Current character: '%c'\n" found;
    let acc =
      if found = 'M' then
        let new_results = find_word h w x y crs in
        List.append new_results acc
      else acc
    in
    if x < w - 1 then walk_crossword h w (x + 1) y crs acc
    else if y < h - 1 then walk_crossword h w 0 (y + 1) crs acc
    else acc

  let print_results results =
    Printf.printf "\nFound %d instances of %s:\n" (List.length results)
      word_to_find;
    List.iter
      (fun result ->
        let x, y = result.start_pos in
        Printf.printf "- At position (%d, %d) going %s\n" x y result.direction)
      results

  let get_result fp =
    let lines = DayOne.read_lines fp in
    let height = List.length lines in
    let width = String.length @@ List.nth lines 0 in
    Printf.printf "Grid dimensions: %d x %d\n" width height;
    let results = walk_crossword height width 0 0 lines [] in
    List.length results / 2

  let get_test_result () = get_result test_file
  let get_result () = get_result file
end

let () =
  Printf.printf "Starting program\n";
  let test = DayFour.get_test_result () in
  Printf.printf "\nTest result: %d\n" test
(* Printf.printf "\n Actual result: %d\n" result *)

let () =
  Printf.printf "Starting program\n";

  let test = DayFour.get_test_result () in
  let result = DayFour.get_result () in
  Printf.printf "\nTest result: %d\n" test;

  Printf.printf "\n Actual result: %d\n" result
