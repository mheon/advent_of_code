(*** Utilities ***)
let string_to_char_list in_str = (List.init (String.length in_str) (String.get in_str))
let char_list_to_string in_list = List.to_seq in_list
                                  |> String.of_seq

let read_file filename = In_channel.with_open_text filename In_channel.input_all

let is_number chr = let code = Char.code chr in
                    if code <= 57 && code >= 48 then
                      true
                    else
                      false

(*** Part 1 ***)
let rec eliminate_extra seq = match seq with
  | '-' :: chr :: rest -> if is_number chr then
                            '-' :: eliminate_extra (chr :: rest)
                          else
                            eliminate_extra (chr :: rest)
  | a :: b :: rest -> if is_number a && not (is_number b) then
                        a :: ',' :: eliminate_extra rest
                      else if is_number a then
                        a :: eliminate_extra (b :: rest)
                      else
                        eliminate_extra (b :: rest)
  | a :: [] -> if is_number a then
                 a :: []
               else
                 []
  | [] -> []

let rec add_all str = string_to_char_list str
                      |> eliminate_extra
                      |> char_list_to_string
                      |> String.split_on_char ','
                      |> List.filter (fun x -> x <> "")
                      |> List.map int_of_string
                      |> List.fold_left (+) 0

let process_input filename = read_file filename
                             |> add_all

(*** Part 2 ***)
(*** Why did I decide to do this the hard way? Why didn't I just grab a JSON library? ***)
(*** I'm committed at this point... but why did I do this to myself ***)
let rec split_and_filter_object char_list acc is_red num_squares = match char_list with
  | '[' :: rest -> if is_red then
                     split_and_filter_object rest [] is_red (num_squares + 1)
                   else
                     split_and_filter_object rest ('[' :: acc) is_red (num_squares + 1)
  | ']' :: rest -> if num_squares = 0 then
                     raise (Invalid_argument "closing square bracket found without open square bracket")
                   else if is_red then
                     split_and_filter_object rest [] is_red (num_squares - 1)
                   else
                     split_and_filter_object rest (']' :: acc) is_red (num_squares - 1)
  | '{' :: rest -> let (inner_acc, remaining) = split_and_filter_object rest [] is_red 0 in
                   split_and_filter_object remaining (('}' :: inner_acc) @ ('{' :: acc)) is_red num_squares
  | '}' :: rest -> (acc, rest)
  | a :: rest -> let new_is_red = is_red || (num_squares = 0 && a = 'd' && (match acc with
                                            | 'e' :: 'r' :: _ -> true
                                            | _ -> false)) in
                 if new_is_red then
                   split_and_filter_object rest [] true num_squares
                 else
                   split_and_filter_object rest (a :: acc) is_red num_squares
  | [] -> (acc, [])

let filter_red str = let char_list = string_to_char_list str in
                     let (final_list, _) = split_and_filter_object char_list [] false 0 in
                     List.rev final_list
                     |> char_list_to_string

let split_into_structs str = let char_list = string_to_char_list str in
                             let rec find_whole_object seq acc num_curlies = match seq with
                               | '{' :: rest -> find_whole_object rest ('{' :: acc) (num_curlies + 1)
                               | '}' :: rest -> let new_num_curlies = num_curlies - 1 in
                                                let new_acc = '}' :: acc in
                                                if new_num_curlies = 0 then
                                                  (new_acc, rest)
                                                else
                                                  find_whole_object rest new_acc new_num_curlies
                               | a_char :: rest -> find_whole_object rest (a_char :: acc) num_curlies
                               | [] -> raise (Invalid_argument "mismatch in number of opening curly braces and closing curly braces") in
                             let rec find_object_start seq objects acc = match seq with
                               | '{' :: rest -> let (found_str, remaining) = find_whole_object rest ['{'] 1 in
                                                let new_objects = if List.is_empty acc then
                                                                    (found_str, true) :: objects
                                                                  else
                                                                    (found_str, true) :: (acc, false) :: objects in
                                                find_object_start remaining new_objects []
                               | a_char :: rest -> find_object_start rest objects (a_char :: acc)
                               | [] -> if List.is_empty acc then
                                         objects
                                       else
                                         (acc, false) :: objects in
                             find_object_start char_list [] []
                             |> List.map (fun (x, y) -> (char_list_to_string (List.rev x), y))

let eliminate_struct_with_red str = split_into_structs str
                                    |> List.rev
                                    |> List.map (fun (x, y) -> if y then (filter_red x, y) else (x, y))
                                    |> List.map (fun (x, y) -> if y then ("{" ^ x ^ "}", y) else (x, y))
                                    |> List.map (fun (x, _) -> x)
                                    |> List.fold_left (^) ""

let process_input_part_2 filename = read_file filename
                                    |> eliminate_struct_with_red
                                    |> add_all
