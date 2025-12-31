(*** Utilities ***)
type operation =
  | On
  | Off
  | Flip

let parse_coord str = let split_coord = String.split_on_char ',' str in
                      match split_coord with
                      | a :: b :: [] -> ((int_of_string a), (int_of_string b))
                      | _ -> raise (Invalid_argument "malformed coordinate")

(*** Part 1 ***)
let create_lights = List.init 1000 (fun _ -> List.init 1000 (fun _ -> false))

let rec count_lights_y lights acc = match lights with
  | a :: rest -> if a then
                   count_lights_y rest (acc + 1)
                 else
                   count_lights_y rest acc
  | [] -> acc

let rec count_lights lights acc = match lights with
  | a :: rest -> count_lights_y a acc
                 |> count_lights rest
  | [] -> acc

let rec set_light_inner_rec op y cur_y lights = match lights with
  | a :: rest -> if y = cur_y then
                   match op with
                   | On -> true :: rest
                   | Off -> false :: rest
                   | Flip -> (not a) :: rest
                 else
                   a :: set_light_inner_rec op y (cur_y + 1) rest
  | [] -> raise (Invalid_argument "reached end of list before target y")

let rec set_light_rec op (x, y) cur_x lights = match lights with
  | a :: rest -> if x = cur_x then
                   set_light_inner_rec op y 0 a :: rest
                 else
                   a :: set_light_rec op (x, y) (cur_x + 1) rest
  | [] -> raise (Invalid_argument "reached end of light before target x")

let set_light op (x, y) lights = if x >= 1000 || y >= 1000 then
                                raise (Invalid_argument "index too large")
                              else if x < 0 || y < 0 then
                                raise (Invalid_argument "index too small")
                              else
                                set_light_rec op (x, y) 0 lights

let rec get_lights_to_set_y x start_y end_y = if start_y > end_y then
                                              []
                                            else
                                              (x, start_y) :: get_lights_to_set_y x (start_y + 1) end_y

let rec get_lights_to_set (start_x, start_y) (end_x, end_y) = if start_x > end_x then
                                                                []
                                                              else
                                                                List.append
                                                                  (get_lights_to_set_y start_x start_y end_y)
                                                                  (get_lights_to_set (start_x + 1, start_y) (end_x, end_y))

let rec set_all_values_inner op to_set lights = match to_set with
  | a :: rest -> set_light op a lights
                 |> set_all_values_inner op rest
  | [] -> lights

let set_all_values op start stop lights = let targets = get_lights_to_set start stop in
                                          set_all_values_inner op targets lights

let parse_operation_line_inner op str_arr lights = match str_arr with
  | a :: _ :: b :: [] -> set_all_values op (parse_coord a) (parse_coord b) lights
  | _ -> raise (Invalid_argument "malformed string array")

let parse_operation_line in_str lights = let split_str = String.split_on_char ' ' in_str in
                                  match split_str with
                                  | "turn" :: rest -> (
                                    match rest with
                                    | "on" :: more -> parse_operation_line_inner On more lights
                                    | "off" :: more -> parse_operation_line_inner Off more lights
                                    | _ -> raise (Invalid_argument "invalid operation")
                                  )
                                  | "toggle" :: rest -> parse_operation_line_inner Flip rest lights
                                  | _ -> raise (Invalid_argument "invalid operation")


let rec process_all_operations_inner lines lights = match lines with
  | line :: rest -> let new_lights = parse_operation_line line lights in
                    process_all_operations_inner rest new_lights
  | [] -> lights


let process_all_operations in_str = let lights = create_lights in
                                    let split_lines = String.split_on_char '\n' in_str in
                                    count_lights (process_all_operations_inner split_lines lights) 0

(*** Part 2 ***)
let make_lights_arr () = Array.make_matrix 1000 1000 0

let rec set_lights_arr_inner op start_y end_y inner_lights = if start_y > end_y then
                                                               inner_lights
                                                             else
                                                               let old_value = Array.get inner_lights start_y in
                                                               let new_value = match op with
                                                                 | On -> 1 + old_value
                                                                 | Off -> if old_value = 0 then old_value else old_value - 1
                                                                 | Flip -> 2 + old_value in
                                                               Array.set inner_lights start_y new_value;
                                                               set_lights_arr_inner op (start_y + 1) end_y inner_lights

let rec set_lights_arr op (start_x, start_y) (end_x, end_y) lights = if start_x > end_x then
                                                                   lights
                                                                 else
                                                                   let _ = set_lights_arr_inner op start_y end_y (Array.get lights start_x) in
                                                                   set_lights_arr op (start_x + 1, start_y) (end_x, end_y) lights

let get_total_illumination lights = Array.map (Array.fold_left (+) 0) lights
                                    |> Array.fold_left ( + ) 0


let perform_one_operation_arr op first last lights = let start_coord = parse_coord first in
                                                     let end_coord = parse_coord last in
                                                     set_lights_arr op start_coord end_coord lights

let process_one_operation_arr in_str lights = let split_str = String.split_on_char ' ' in_str in
                                                   match split_str with
                                                   | "turn" :: "on" :: first :: "through" :: last :: [] -> perform_one_operation_arr On first last lights
                                                   | "turn" :: "off" :: first :: "through" :: last :: [] -> perform_one_operation_arr Off first last lights
                                                   | "toggle" :: first :: "through" :: last :: [] -> perform_one_operation_arr Flip first last lights
                                                   | _ -> raise (Invalid_argument "invalid input format")

let rec process_all_operations_inner lines lights = match lines with
  | line :: rest -> (
     let _ = process_one_operation_arr line lights in
     process_all_operations_inner rest lights
  )
  | [] -> ()

let process_all_operations_arr input = let lights = make_lights_arr () in
                                       let lines = String.split_on_char '\n' input in
                                       process_all_operations_inner lines lights;
                                       get_total_illumination lights
