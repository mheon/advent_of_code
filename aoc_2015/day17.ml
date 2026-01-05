let find_minimum_elements list target = let sorted_list = List.rev @@ List.sort ( compare ) list in
                                        let rec find_min elements acc = match elements with
                                          | e :: rest -> if acc >= target then
                                                           0
                                                         else
                                                           1 + find_min rest (acc + e)
                                          | [] -> raise (Invalid_argument "no possible combination of elements reaches target") in
                                        find_min sorted_list 0

let rec find_valid_combinations elements k target to_test = if k = 0 then
                                                              (if (List.fold_left ( + ) 0 to_test) = target then
                                                                1
                                                              else
                                                                0)
                                                            else
                                                              match elements with
                                                              | [] -> 0
                                                              | e :: rest -> find_valid_combinations rest (k-1) target (e :: to_test) + find_valid_combinations rest k target to_test

let find_all_valid_combinations elements target only_min = let minimum_k = find_minimum_elements elements target in
                                                  let rec try_all_k cur_k max_k = if cur_k > max_k then
                                                                                    0
                                                                                  else
                                                                                    let cur_k_combinations = find_valid_combinations elements cur_k target [] in
                                                                                    if only_min && cur_k_combinations > 0 then
                                                                                       cur_k_combinations
                                                                                    else
                                                                                      cur_k_combinations + try_all_k (cur_k + 1) max_k in
                                                  try_all_k minimum_k @@ List.length elements

let part_1 str target = let elements = List.map int_of_string @@ String.split_on_char '\n' str in
                        find_all_valid_combinations elements target false

let part_2 str target = let elements = List.map int_of_string @@ String.split_on_char '\n' str in
                        find_all_valid_combinations elements target true
