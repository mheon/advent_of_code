let get_floor in_str = String.fold_left
                         (fun acc c -> match c with
                                       | '(' -> acc + 1
                                       | ')' -> acc - 1
                                       | _ -> acc)
                         0 in_str

let rec get_first_basement_floor_inner in_str acc point = let (new_acc, rest) = match in_str with
                                                            | c :: r -> ( match c with
                                                                           | '(' -> (acc + 1, r)
                                                                           | ')' -> (acc - 1, r)
                                                                           | _ -> (acc, r) )
                                                            | [] -> raise (Invalid_argument "never reaches basement")
                                                          in
                                                          if new_acc < 0 then
                                                            point
                                                          else
                                                            get_first_basement_floor_inner rest new_acc (point + 1)

let get_first_basement_floor in_str = get_first_basement_floor_inner (List.init (String.length in_str) (String.get in_str)) 0 1
