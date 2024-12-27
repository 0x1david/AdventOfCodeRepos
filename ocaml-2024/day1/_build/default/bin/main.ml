let file = "bin/input"
let test_file = "bin/test"
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
      else process_chars (pos + 1) [] @ [ chars_to_int (List.rev current_num) ]
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

let () =
  Printf.printf "Starting program\n";
  let pairs = file |> read_lines |> separate_lines [] |> create_pairs [] in

  let left = List.sort compare @@ fst_list [] pairs in
  let right = List.sort compare @@ snd_list [] pairs in
  let sorted_pairs = List.combine left right in
  let result = count_distance 0 sorted_pairs in
  let sim_score = count_all_sim_scores right left in

  Printf.printf "\nFinal result: %d\n" result;
  print_int result;
  Printf.printf "\nFinal Sim Score: %d\n" sim_score;
  print_int result;

  Printf.printf "Starting Test \n";
  let pairs = test_file |> read_lines |> separate_lines [] |> create_pairs [] in

  let left = List.sort compare @@ fst_list [] pairs in
  let right = List.sort compare @@ snd_list [] pairs in
  let sorted_pairs = List.combine left right in
  let result = count_distance 0 sorted_pairs in
  let sim_score = count_all_sim_scores right left in

  Printf.printf "\nFinal result: %d\n" result;
  print_int result;
  Printf.printf "\nFinal Sim Score: %d\n" sim_score;
  print_int result
