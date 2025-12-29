(*** Utilities ***)
let string_to_char_list in_str = (List.init (String.length in_str) (String.get in_str))

(*** Part 1 ***)
let has_3_vowels input = let count = List.map (fun x -> match x with
                                             | 'a' | 'e' | 'i' | 'o' | 'u' -> 1
                                             | _ -> 0) input
                                     |> List.fold_left ( + ) 0
                         in
                         if count < 3 then
                           false
                         else
                           true

let rec has_repeated_letter char_list last_char = match char_list with
  | c :: rest -> if c = last_char then
                   true
                 else
                   has_repeated_letter rest c
  | [] -> false

let match_two_char_sequence (a, b) (c, d) = if a = c && b = d then true else false

let rec check_all_two_char_sequences (a, b) bad_sequences = match bad_sequences with
  | seq :: rest -> if (match_two_char_sequence (a, b) seq) = true then
                     false
                   else
                     check_all_two_char_sequences (a, b) rest
  | [] -> true

let rec does_not_have_two_char_sequence char_list last_char bad_sequences = match char_list with
  | c :: rest -> if check_all_two_char_sequences (last_char, c) bad_sequences = false then
                   false
                 else
                   does_not_have_two_char_sequence rest c bad_sequences
  | [] -> true

let check_all_requirements input = let char_list = string_to_char_list input in
                                   let check_1 = has_3_vowels char_list in
                                   let check_2 = has_repeated_letter char_list ' ' in
                                   let check_3 = does_not_have_two_char_sequence char_list ' ' [('a', 'b'); ('c', 'd'); ('p', 'q'); ('x', 'y')] in
                                   check_1 && check_2 && check_3

let check_all_strings input = String.split_on_char '\n' input
                              |> List.map check_all_requirements
                              |> List.fold_left (fun acc x -> if x = true then acc + 1 else acc) 0

(*** Part 2 ***)
let rec check_pair_non_overlapping (a, b) to_check = match to_check with
  | c :: d :: rest -> if a = c && b = d then
                        true
                      else
                        check_pair_non_overlapping (a, b) (d :: rest)
  | _ -> false

let rec check_all_possible_pairs to_check = match to_check with
  | a :: b :: rest -> if check_pair_non_overlapping (a, b) rest then
                        true
                      else
                        check_all_possible_pairs (b :: rest)
  | _ -> false

let rec check_letter_repeated_with_middle to_check = match to_check with
  | a :: b :: c :: rest -> if a = c then
                             true
                           else
                             check_letter_repeated_with_middle (b :: c :: rest)
  | _ -> false

let check_all_requirements_part_2 input = let char_list = string_to_char_list input in
                                          let check_1 = check_all_possible_pairs char_list in
                                          let check_2 = check_letter_repeated_with_middle char_list in
                                          check_1 && check_2

let check_all_strings_part_2 input = String.split_on_char '\n' input
                                     |> List.map check_all_requirements_part_2
                                     |> List.fold_left (fun acc x -> if x = true then acc + 1 else acc) 0
