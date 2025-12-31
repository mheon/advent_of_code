(*** Utilities ***)
let string_to_char_list in_str = (List.init (String.length in_str) (String.get in_str))

(*** Part 1 ***)
let rec find_consecutive_characters char_list = match char_list with
  | a_char :: rest -> let rec match_further_chars c_list instances = match c_list with
                        | test_char :: more -> if test_char = a_char then
                                                 match_further_chars more (instances + 1)
                                               else
                                                 (instances, c_list)
                        | [] -> (instances, c_list) in
                      let (instances, remaining) = (match_further_chars rest 1) in
                      (a_char, instances) :: find_consecutive_characters remaining
  | [] -> []

let rec convert_to_char_sequence list = match list with
  | (a_char, instances) :: rest -> (string_to_char_list (string_of_int instances)) @ (a_char :: (convert_to_char_sequence rest))
  | [] -> []

let rec handle_iteration input remaining = if remaining = 0 then
                                             input
                                           else
                                             let new_input = find_consecutive_characters input
                                                             |> convert_to_char_sequence in
                                             handle_iteration new_input (remaining - 1)

let process_input_part_1 input iterations = let split_input = string_to_char_list input in
                                 handle_iteration split_input iterations
                                 |> List.length

(*** Part 2 was just added iteration count, no need for changes ***)
