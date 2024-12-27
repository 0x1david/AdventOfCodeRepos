let is_digit d = match d with '0' .. '9' -> true | _ -> false
let file = "bin/input"
let test_file = "bin/test"

let read_lines filename =
  Printf.printf "\nStarting to read file: %s\n" filename;
  let ichan = open_in filename in
  let try_read () = try Some (input_line ichan) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some value ->
        Printf.printf "Read line: %s\n" value;
        loop @@ (value :: acc)
    | None ->
        close_in ichan;
        let result = List.rev acc in
        Printf.printf "Finished reading. Total lines: %d\n" (List.length result);
        result
  in
  loop []

let chars_to_int chars =
  let result =
    chars |> List.map Char.escaped |> String.concat "" |> int_of_string
  in
  Printf.printf "Converting chars to int: %d\n" result;
  result

let separate_line acc line =
  Printf.printf "\nProcessing line: %s\n" line;
  let len = String.length line in
  let rec process_chars pos current_num =
    Printf.printf "Position: %d, Current number chars: %s\n" pos
      (String.concat "" (List.map Char.escaped current_num));
    if pos >= len then (
      match current_num with
      | [] ->
          Printf.printf "End of line, no current number\n";
          acc
      | nums ->
          let result = acc @ [ chars_to_int (List.rev nums) ] in
          Printf.printf "End of line, adding number. Accumulator: %s\n"
            (String.concat ", " (List.map string_of_int result));
          result)
    else
      let c = line.[pos] in
      if is_digit c then (
        Printf.printf "Found digit: %c\n" c;
        process_chars (pos + 1) (c :: current_num))
      else if List.is_empty current_num then (
        Printf.printf "Found non-digit: %c, no current number\n" c;
        process_chars (pos + 1) [])
      else (
        Printf.printf "Found non-digit: %c, completing number\n" c;
        process_chars (pos + 1) [] @ [ chars_to_int (List.rev current_num) ])
  in
  let result = process_chars 0 [] in
  Printf.printf "Line processing complete. Numbers found: %s\n"
    (String.concat ", " (List.map string_of_int result));
  result

let rec separate_lines acc lines =
  Printf.printf "\nProcessing lines. Current accumulator: %s\n"
    (String.concat ", " (List.map string_of_int acc));
  match lines with
  | [] ->
      Printf.printf "No more lines to process\n";
      acc
  | line :: rest ->
      let numbers = separate_line acc line in
      Printf.printf "Line processed. Updated accumulator: %s\n"
        (String.concat ", " (List.map string_of_int numbers));
      separate_lines numbers rest

let rec create_pairs acc all_ints =
  Printf.printf "\nCreating pairs. Input numbers: %s\n"
    (String.concat ", " (List.map string_of_int all_ints));
  match all_ints with
  | [] ->
      Printf.printf "No more numbers to pair\n";
      acc
  | [ _ ] ->
      Printf.printf "Single number remaining, ignoring\n";
      acc
  | first :: second :: rest ->
      let new_acc = (second, first) :: acc in
      Printf.printf "Created pair: (%d, %d)\n" first second;
      create_pairs new_acc rest

let rec count_distance sum pairs =
  Printf.printf "\nCounting distances. Current sum: %d\n" sum;
  match pairs with
  | [] ->
      Printf.printf "Final sum: %d\n" sum;
      sum
  | (left, right) :: rest ->
      Printf.printf "Processing pair: (%d, %d)\n" left right;
      count_distance (abs (left - right) + sum) rest

let () =
  Printf.printf "Starting program\n";
  let result =
    file |> read_lines |> separate_lines [] |> create_pairs []
    |> count_distance 0
  in
  Printf.printf "\nFinal result: %d\n" result;
  print_int result;

  let result =
    test_file |> read_lines |> separate_lines [] |> create_pairs []
    |> count_distance 0
  in
  Printf.printf "\nTest result: %d\n, Expected 11" result;
  print_int result
