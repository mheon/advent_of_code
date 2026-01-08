(*** Part 1 ***)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let is_lowercase c = let code = Char.code c in
                     code >= 97 && code <= 122

let split_into_elements str = let char_list = List.init (String.length str) (String.get str) in
                              let rec handle_chars list elements acc =
                                let acc_to_string a = String.of_seq @@ List.to_seq @@ List.rev a in
                                match list with
                                | c :: rest -> if is_lowercase c then
                                                 handle_chars rest elements (c :: acc)
                                               else
                                                 let new_elements = if List.length acc = 0 then
                                                                      elements
                                                                    else
                                                                      acc_to_string acc :: elements in
                                                 handle_chars rest new_elements [c]
                                | [] -> List.rev @@ acc_to_string acc :: elements in
                              handle_chars char_list [] []

let try_substitutions elements substitutions = let base_set = StringSet.empty in
                                               let rec try_each_substitution head tail substitutions set = match substitutions with
                                                 | [] -> set
                                                 | sub :: rest -> let new_set = StringSet.add ((List.fold_left (^) "" head) ^ sub ^ List.fold_left (^) "" tail) set in
                                                                  try_each_substitution head tail rest new_set in
                                               let rec try_sub_inner elements head set = match elements with
                                                 | e :: rest -> (match StringMap.find_opt e substitutions with
                                                                | Some possibilities -> let new_set = try_each_substitution head rest possibilities set in
                                                                                        try_sub_inner rest (head @ [e]) new_set
                                                                | None -> try_sub_inner rest (head @ [e]) set)
                                                 | [] -> set in
                                               try_sub_inner elements [] base_set

let add_to_substitutions element substitution map = let (inner_list, new_map) = match StringMap.find_opt element map with
                                                      | Some list -> (list, StringMap.remove element map)
                                                      | None -> ([], map) in
                                                    StringMap.add element (substitution :: inner_list) new_map

let parse_input str = let split_str = String.split_on_char '\n' str in
                      let substitutions_list = List.filter (fun x -> String.contains x '=') split_str in
                      let base_map = StringMap.empty in
                      let handle_substitution line map = match String.split_on_char ' ' line with
                        | element :: "=>" :: substitution :: [] -> add_to_substitutions element substitution map
                        | _ -> raise (Invalid_argument "unrecognized input format") in
                      let rec handle_all_substitutions list map = match list with
                        | line :: rest -> handle_all_substitutions rest (handle_substitution line map)
                        | [] -> map in
                      let substitutions = handle_all_substitutions substitutions_list base_map in
                      let elements = split_into_elements @@ List.hd @@ List.rev split_str in
                      (elements, substitutions)

let part_1 str = let (elements, substitutions) = parse_input str in
                 List.length @@ StringSet.to_list @@ try_substitutions elements substitutions

(*** Part 2 ***)
(*** After some failed experiments with BFS and CYK... I gave up and looked up the trick: https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4h7ji/ ***)
let part_2 str = let (elements, _) = parse_input str in
                 let count_elements str list = List.length @@ List.filter (fun x -> x = str) list in
                 List.length elements - count_elements "Rn" elements - count_elements "Ar" elements - (2 * count_elements "Y" elements) - 1
