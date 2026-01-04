(*** Utilities ***)

type ingredient = {
    name: string;
    capacity: int;
    durability: int;
    flavor: int;
    texture: int;
    calories: int;
  }

let compute_points ingredients count_calories = let rec compute_points_inner ingredients = match ingredients with
                                   | (ingredient, quantity) :: rest -> let (cap, durability, flavor, texture, cals) = compute_points_inner rest in
                                                                       (cap + ingredient.capacity * quantity, durability + ingredient.durability * quantity, flavor + ingredient.flavor * quantity, texture + ingredient.texture * quantity, cals + ingredient.calories * quantity)
                                   | [] -> (0, 0, 0, 0, 0) in
                                 let (cap, durability, flavor, texture, calories) = compute_points_inner ingredients in
                                 if cap < 0 || durability < 0 || flavor < 0 || texture < 0 then
                                   0
                                 else if count_calories && calories <> 500 then
                                   -1
                                 else
                                   cap * durability * flavor * texture

(*** Part 1 ***)

(* Would be best to do calorie counting here, can abort early if we hit over 500 instead of continuing, but this required less refactoring... *)
let rec get_best_mix ingredients cumulative to_try count_calories =
  if cumulative > 100 then
    (-1, to_try)
  else
    match ingredients with
    | i :: rest -> let cur_best_score = ref (-1) in
                   let ingredients_in_score = ref [] in
                   for count = 0 to 100 do
                     let (new_score, new_ingredients) = if count + cumulative <= 100 then
                                                          get_best_mix rest (cumulative + count) ((i, count) :: to_try) count_calories
                                                        else
                                                          (-1, to_try) in
                     if new_score > !cur_best_score then (
                       cur_best_score := new_score;
                       ingredients_in_score := new_ingredients
                     );
                   done;
                   (!cur_best_score, !ingredients_in_score)
    | [] -> if cumulative <> 100 then
              (-1, to_try)
            else
              (compute_points to_try count_calories, to_try)

let strip_last_char str = String.sub str 0 ((String.length str) - 1)

let parse_input str = let lines = String.split_on_char '\n' str in
                      let parse_line line = match String.split_on_char ' ' line with
                        | name :: "capacity" :: cap :: "durability" :: dur :: "flavor" :: flav :: "texture" :: text :: "calories" :: cal :: [] ->
                           {
                             name = strip_last_char name;
                             capacity = int_of_string (strip_last_char cap);
                             durability = int_of_string (strip_last_char dur);
                             flavor = int_of_string (strip_last_char flav);
                             texture = int_of_string (strip_last_char text);
                             calories = int_of_string cal;
                           }
                        | _ -> raise (Invalid_argument "bad input format") in
                      List.map parse_line lines

let part_1 str = let ingredients = parse_input str in
                 get_best_mix ingredients 0 [] false

(*** Part 2 ***)
let part_2 str = let ingredients = parse_input str in
                 get_best_mix ingredients 0 [] true
