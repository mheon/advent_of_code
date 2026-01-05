let get_base_grid size = Array.make_matrix size size 0

let get_light_value arr (x, y) = let inner_arr = Array.get arr x in
                                 Array.get inner_arr y

let set_light_value arr (x, y) num = let inner_arr = Array.get arr x in
                                     Array.set inner_arr y num

let get_adjacent_lights (x, y) size = List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < size && y < size)
                                        [(x + 1, y); (x + 1, y + 1); (x + 1, y - 1); (x, y + 1); (x, y - 1); (x - 1, y); (x - 1, y + 1); (x - 1, y - 1)]

let count_lights arr = Array.to_list arr
                       |> List.map (fun x -> List.fold_left (+) 0 @@ Array.to_list x)
                       |> List.fold_left (+) 0

let set_corners arr size = let max = size - 1 in
                           set_light_value arr (0, 0) 1;
                           set_light_value arr (0, max) 1;
                           set_light_value arr (max, 0) 1;
                           set_light_value arr (max, max) 1


let handle_one_light (x, y) old_arr new_arr size = let num_adjacent = get_adjacent_lights (x, y) size
                                                                 |> List.map (get_light_value old_arr)
                                                                 |> List.fold_left ( + ) 0 in
                                              let cur_state = get_light_value old_arr (x, y) in
                                              let new_state = if (cur_state = 1 && num_adjacent = 2) || num_adjacent = 3 then
                                                                1
                                                              else
                                                                0 in
                                              set_light_value new_arr (x, y) new_state

let handle_one_step arr size corners = let new_arr = get_base_grid size in
                                       for x = 0 to (size - 1) do
                                         for y = 0 to (size - 1) do
                                           handle_one_light (x, y) arr new_arr size
                                         done
                                       done;
                                       if corners then
                                         set_corners new_arr size;
                                       new_arr

let rec handle_n_steps base_arr size steps corners = if steps = 0 then
                                                       base_arr
                                                     else
                                                       let new_arr = handle_one_step base_arr size corners in
                                                       handle_n_steps new_arr size (steps - 1) corners

let handle_input str size = let arr = get_base_grid size in
                            let handle_line line x = let to_set = List.init (String.length line) (String.get line)
                                                                  |> List.map (fun x -> match x with | '#' -> 1 | _ -> 0) in
                                                     for y = 0 to ((List.length to_set) - 1) do
                                                       set_light_value arr (x, y) (List.nth to_set y)
                                                     done in
                            let rec handle_all_lines lines x = match lines with
                              | line :: rest -> handle_line line x; handle_all_lines rest (x + 1)
                              | [] -> () in
                            let lines = String.split_on_char '\n' str in
                            handle_all_lines lines 0;
                            arr

let part_1 input size steps = let base_arr = handle_input input size in
                              handle_n_steps base_arr size steps false
                              |> count_lights

let part_2 input size steps = let base_arr = handle_input input size in
                              set_corners base_arr size;
                              handle_n_steps base_arr size steps true
                              |> count_lights
