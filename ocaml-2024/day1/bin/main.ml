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

  let file = "bin/input_two"
  let test_file = "bin/test_two"
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

  let file = "bin/input_three"
  let test_file = "bin/test_three"
  let pat = Str.regexp "mul(\\([1-9][0-9]*\\),\\([1-9][0-9]*\\))"

  let find_uncorrupted pat haystack =
    let rec find_all pos acc =
      try
        let pos = Str.search_forward pat haystack pos in
        let match1 = int_of_string @@ Str.matched_group 1 haystack in
        let match2 = int_of_string @@ Str.matched_group 2 haystack in
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

let () =
  Printf.printf "Starting program\n";
  let result = DayThree.get_result () in
  let test = DayThree.get_test_result () in

  Printf.printf "\nTest result: %d\n" test;
  Printf.printf "\n Actual result: %d\n" result
