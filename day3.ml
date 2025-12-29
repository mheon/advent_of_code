(*** Utilities ***)
module IntSet = Set.Make(Int)

let new_empty_coords = [(0, IntSet.singleton 0)]

let rec get_coords_length_rec coords acc = match coords with
  | (_, set) :: rest -> let new_acc = acc + List.length (IntSet.to_list set) in
                        get_coords_length_rec rest new_acc
  | [] -> acc

let string_to_char_list in_str = (List.init (String.length in_str) (String.get in_str))

(*** Part 1 ***)
let rec add_coord (x, y) coords = match coords with
  | (check_x, set) :: rest -> if check_x = x then
                                (check_x, IntSet.add y set) :: rest
                              else if check_x > x then
                                (x, IntSet.singleton y) :: (check_x, set) :: rest
                              else
                                (check_x, set) :: add_coord (x, y) rest
  | [] -> (x, IntSet.singleton y) :: []

let navigate_one_step d (x, y) coords = let new_coord = match d with
  | '^' -> (x, y + 1)
  | '>' -> (x + 1, y)
  | 'v' -> (x, y - 1)
  | '<' -> (x - 1, y)
  | _ -> raise (Invalid_argument ("Not a valid character: " ^ (String.make 1 d))) in
                                        (new_coord, (add_coord new_coord coords))

let rec navigate_all_steps instructions (x, y) coords = match instructions with
  | d :: rest -> let (new_pos, new_coords) = navigate_one_step d (x, y) coords in
                 navigate_all_steps rest new_pos new_coords
  | [] -> coords

let navigate in_str = let instructions = string_to_char_list in_str in
                      let base_coords = new_empty_coords in
                      get_coords_length_rec (navigate_all_steps instructions (0, 0) base_coords) 0

(*** Part 2 ***)
let rec navigate_two_santas_all_steps instructions (x1, y1) (x2, y2) coords = match instructions with
  | d1 :: d2 :: rest -> let (new_pos_1, new_coords_1) = navigate_one_step d1 (x1, y1) coords in
                        let (new_pos_2, new_coords_2) = navigate_one_step d2 (x2, y2) new_coords_1 in
                        navigate_two_santas_all_steps rest new_pos_1 new_pos_2 new_coords_2
  | d :: [] -> navigate_all_steps instructions (x1, y1) coords
  | [] -> coords

let navigate_two_santas in_str = let instructions = string_to_char_list in_str in
                          let base_coords = new_empty_coords in
                          get_coords_length_rec (navigate_two_santas_all_steps instructions (0, 0) (0, 0) base_coords) 0
