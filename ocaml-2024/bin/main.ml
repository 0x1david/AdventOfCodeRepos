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

  let file = "bin/input/input_four"
  let test_file = "bin/test/test_four"
  let word_to_find = "xmas"
  let left x y = (x - 1, y)
  let right x y = (x + 1, y)
  let up x y = (x, y - 1)
  let down x y = (x, y + 1)
  let up_left x y = (x - 1, y - 1)
  let up_right x y = (x + 1, y - 1)
  let down_left x y = (x - 1, y + 1)
  let down_right x y = (x + 1, y + 1)

  let check_letter x y crs letter =
    let col = List.nth crs x in
    let found = String.get col y in
    found = letter

  let rec walk_check f x y crs letter_pos =
    if letter_pos >= String.length word_to_find then true
    else
      let new_x, new_y = f x y in
      let searched_letter = String.get word_to_find letter_pos in
      let found = check_letter new_x new_y crs searched_letter in
      if found then walk_check f new_x new_y crs @@ (letter_pos + 1) else false

  let find_word h w x y crs =
    let word_len = String.length word_to_find in
    let directions =
      [
        (left, x >= word_len);
        (right, w - x >= word_len);
        (up, y >= word_len);
        (down, h - y >= word_len);
        (up_left, x >= word_len && y >= word_len);
        (up_right, w - x >= word_len && y >= word_len);
        (down_left, x >= word_len && h - y >= word_len);
        (down_right, w - x >= word_len && h - y >= word_len);
      ]
    in
    List.fold_left
      (fun acc (f, cond) ->
        if cond && walk_check f x y crs 1 then acc + 1 else acc)
      0 directions

  let rec walk_crossword h w x y crs acc =
    let col = List.nth crs x in
    let found = String.get col y in
    let acc = if found = 'x' then acc + find_word h w x y crs else acc in

    if x <= w then walk_crossword h w (x + 1) y crs acc
    else if y <= h then walk_crossword h w 0 (y + 1) crs acc
    else acc

  let _get_result fp =
    let lines = DayOne.read_lines fp in
    let height = List.length lines in
    let width = String.length @@ List.nth lines 0 in
    walk_crossword height width 0 0 lines 0

  let get_test_result () = _get_result test_file
  let get_result () = _get_result file
end

let () =
  Printf.printf "Starting program\n";
  let test = DayFour.get_test_result () in

  Printf.printf "\nTest result: %d\n" test
(* Printf.printf "\n Actual result: %d\n" result *)
