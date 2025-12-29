(*** Utilities ***)

let get_min l = List.fold_left (fun acc x -> if x < acc then x else acc) (List.hd l) l
let get_lwh in_str = match String.split_on_char 'x' in_str with
    | l :: w :: h :: [] -> ((int_of_string l), (int_of_string w), (int_of_string h))
    | _ -> raise (Invalid_argument ("not in LxWxH format: " ^ in_str))

(*** Part 1 ***)
let get_area_for_present l w h = let side_1 = l * w in
                                 let side_2 = w * h in
                                 let side_3 = h * l in
                                 2 * side_1 + 2 *  side_2 + 2 * side_3 + get_min [side_1; side_2; side_3]

let get_area_for_present_string in_str = let (l, w, h) = get_lwh in_str in
                                         get_area_for_present l w h

let rec get_area_for_all_presents_list list acc = match list with
  | line :: rest -> let new_acc = (get_area_for_present_string line) + acc in
                    get_area_for_all_presents_list rest new_acc
  | [] -> acc

let get_area_for_all_presents str = get_area_for_all_presents_list (String.split_on_char '\n' str) 0

(*** Part 2 ***)
let get_ribbon_for_present l w h = let sides = [(2 * l + 2 * w); (2 * l + 2 * h); (2 * w + 2 * h)] in
                                   let ribbon = l * w * h in
                                   get_min sides + ribbon

let get_ribbon_for_present_string in_str = let (l, w, h) = get_lwh in_str in
                                           get_ribbon_for_present l w h

let rec get_ribbon_all_presents_list list acc = match list with
  | line :: rest -> let new_acc = (get_ribbon_for_present_string line) + acc in
                    get_ribbon_all_presents_list rest new_acc
  | [] -> acc

let get_ribbon_all_presents in_str = get_ribbon_all_presents_list (String.split_on_char '\n' in_str) 0
