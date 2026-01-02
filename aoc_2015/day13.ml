(*** Utilities ***)
module StringMap = Map.Make(String)

type person = {
    name: string;
    adjustments: int StringMap.t
  }

let empty_people_map () = StringMap.empty

let get_person name people = StringMap.find name people

let update_person person people = let updated_people = match StringMap.find_opt person.name people with
                                    | Some s -> StringMap.remove s.name people
                                    | None -> people in
                                  StringMap.add person.name person updated_people

let add_adjacency source target weight people = let cur_adjustments = match StringMap.find_opt source people with
                                                  | Some p -> p.adjustments
                                                  | None -> StringMap.empty in
                                                let updated_person = {
                                                    name = source;
                                                    adjustments = StringMap.add target weight cur_adjustments;
                                                  } in
                                                update_person updated_person people

let get_adjacency person target = StringMap.find target person.adjustments

let parse_adjacency_line line people = match String.split_on_char ' ' line with
  | source :: "would" :: action :: weight :: "happiness" :: "units" :: "by" :: "sitting" :: "next" :: "to" :: target :: [] ->
     let int_weight = int_of_string weight in
     let final_weight = match action with
       | "gain" -> int_weight
       | "lose" -> -1 * int_weight
       | a -> raise (Invalid_argument ("unrecognized action " ^ a)) in
     let final_target = String.sub target 0 ((String.length target) - 1) in (* Input formatted as sentences, so end with a period *)
     add_adjacency source final_target final_weight people
  | "" :: [] -> people
  | _ -> raise (Invalid_argument "unrecognized input format")

let parse_input str = let rec parse_input_inner lines people = match lines with
                        | line :: rest -> parse_input_inner rest (parse_adjacency_line line people)
                        | [] -> people in
                      parse_input_inner (String.split_on_char '\n' str) (empty_people_map ())

let get_happiness arrangement people = if List.length arrangement = 0 then
                                         0
                                       else
                                         let rec get_happiness_inner prev list = match list with
                                           | a :: rest -> let cur_person = get_person a people in
                                                          let next_person = match rest with
                                                            | b :: _ -> b
                                                            | [] -> List.hd arrangement in
                                                          let prev_person = match prev with
                                                            | Some p -> p
                                                            | None -> List.nth arrangement ((List.length arrangement) - 1) in
                                                          (get_adjacency cur_person next_person) + (get_adjacency cur_person prev_person) + get_happiness_inner (Some a) rest
                                           | [] -> 0 in
                                         get_happiness_inner None arrangement


(*** Part 1 ***)
let generate_all_permutations list = let len = List.length list in
                                     if len = 0 then
                                       list :: []
                                     else
                                       let perm_array = Array.of_list list in
                                       let swap arr ind1 ind2 = let tmp = Array.get arr ind1 in Array.set arr ind1 (Array.get arr ind2); Array.set arr ind2 tmp in
                                       let rec generate_perm_inner k = if k = 1 then
                                                                         List.of_seq (Array.to_seq perm_array) :: []
                                                                       else
                                                                         let new_k = k - 1 in
                                                                         let perms = ref [] in
                                                                         for i = 0 to new_k do
                                                                           perms := !perms @ (generate_perm_inner new_k);
                                                                           (if k mod 2 = 0 then
                                                                             swap perm_array i new_k
                                                                           else
                                                                             swap perm_array 0 new_k);
                                                                         done;
                                                                         !perms in
                                       generate_perm_inner len

let rec find_highest_happiness cur_best permutations = match permutations with
  | (p, cost) :: rest -> let new_best = match cur_best with
                           | Some (old_p, old_cost) -> if cost > old_cost then
                                                         (p, cost)
                                                       else
                                                         (old_p, old_cost)
                           | None -> (p, cost) in
                         find_highest_happiness (Some new_best) rest
  | [] -> cur_best

let find_optimal_seating in_str = let people = parse_input in_str in
                                  let permutations = StringMap.bindings people
                                                     |> List.map (fun (x, y) -> x)
                                                     |> generate_all_permutations
                                                     |> List.map (fun p -> (p, get_happiness p people)) in
                                  match find_highest_happiness None permutations with
                                  | Some (p, cost) -> print_endline ("Found best happiness as " ^ string_of_int cost ^ " with arrangement " ^ (List.fold_left (fun x y -> x ^ "," ^ y) "" p))
                                  | None -> print_endline "Found no permutations"

(*** Part 2 ***)
let add_empty_person people = let myself = "myself" in
                              let current_people = StringMap.bindings people
                                                   |> List.map (fun (x, y) -> x) in
                              let rec add_to_all list people_map = match list with
                                | name :: rest -> add_adjacency myself name 0 people_map
                                                  |> add_adjacency name myself 0
                                                  |> add_to_all rest
                                | [] -> people_map in
                              add_to_all current_people people

let find_optimal_seating_part_2 in_str = let people = parse_input in_str
                                                      |> add_empty_person in
                                         match StringMap.bindings people
                                               |> List.map (fun (x, y) -> x)
                                               |> generate_all_permutations
                                               |> List.map (fun p -> (p, get_happiness p people))
                                               |> find_highest_happiness None with
                                         | Some (p, cost) -> print_endline ("Found best happiness as " ^ string_of_int cost ^ " with arrangement " ^ (List.fold_left (fun x y -> x ^ "," ^ y) "" p))
                                         | None -> print_endline "Found no permutations"
