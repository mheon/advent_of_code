(*** Utilities ***)
let string_to_char_list in_str = (List.init (String.length in_str) (String.get in_str))

(*** Part 1 ***)
let increment_password pw = let rec increment_inner char_list = match char_list with
                              | a_char :: rest -> let incremented = (Char.code a_char) + 1 in
                                                  if incremented >= 123 then
                                                    'a' :: increment_inner rest
                                                  else
                                                    Char.chr incremented :: rest
                              | [] -> [] in
                            List.rev pw
                            |> increment_inner
                            |> List.rev

let rec check_increasing_sequence seq = match seq with
  | char_a :: char_b :: char_c :: rest -> let val1 = Char.code char_a in
                                          let val2 = Char.code char_b in
                                          let val3 = Char.code char_c in
                                          if val3 - val2 = 1 && val2 - val1 = 1 then
                                            true
                                          else
                                            check_increasing_sequence (char_b :: char_c :: rest)
  | _ -> false

let check_has_iol char_list = List.mem 'i' char_list || List.mem 'o' char_list || List.mem 'l' char_list

let check_two_pairs seq = let rec count_num_pairs seq = match seq with
                           | a :: b :: rest -> if a = b then
                                                 1 + count_num_pairs rest
                                               else
                                                 count_num_pairs (b :: rest)
                           | _ -> 0 in
                         if (count_num_pairs seq) >= 2 then
                           true
                         else
                           false

let check_pw_requirements seq = check_increasing_sequence seq && (not (check_has_iol seq)) && check_two_pairs seq

(*** Also used for part 2 ***)
let find_next_valid_password password = let rec next_password p = let new_pw = increment_password p in
                                                            if check_pw_requirements new_pw then
                                                              new_pw
                                                            else
                                                              next_password new_pw in
                                  string_to_char_list password
                                  |> next_password
                                  |> List.to_seq
                                  |> String.of_seq
