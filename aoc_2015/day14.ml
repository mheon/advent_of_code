(*** Utilities ***)

type reindeer = {
    name: string;
    speed: int;
    fly_period: int;
    rest_period: int;
  }

let get_distance_traveled r seconds = let period = r.fly_period + r.rest_period in
                                      let num_cycles = seconds / period in
                                      let partial_period = seconds mod period in
                                      let partial_distance = if partial_period > r.fly_period then r.fly_period else partial_period in
                                      (num_cycles * r.fly_period + partial_distance) * r.speed

(*** Part 1 ***)
let process_input str = let process_input_inner line = let split = String.split_on_char ' ' line in
                                                       let name = List.hd split in
                                                       let nums = List.filter (fun x -> Option.is_some (int_of_string_opt x)) split
                                                                  |> List.map ( int_of_string ) in
                                                       match nums with
                                                       | speed :: fly :: rest :: [] ->
                                                          {
                                                            name = name;
                                                            speed = speed;
                                                            fly_period = fly;
                                                            rest_period = rest;
                                                          }
                                                       | _ -> raise (Invalid_argument "unknown input format") in
                        String.split_on_char '\n' str
                        |> List.map ( process_input_inner )

let get_current_winner reindeers seconds = List.map (fun x -> (x.name, get_distance_traveled x seconds)) reindeers
                           |> List.fold_left (fun (cur_name, cur_distance) (name, distance) -> if distance > cur_distance then (name, distance) else (cur_name, cur_distance)) ("", 0)

let part_1 input seconds = let reindeers = process_input input in
                           get_current_winner reindeers seconds


(*** Part 2 ***)
module StringMap = Map.Make(String)

let update_winners new_winner winners = let (value, updated_map) = match StringMap.find_opt new_winner winners with
                                          | Some v -> (v + 1, StringMap.remove new_winner winners)
                                          | None -> (1, winners) in
                                        StringMap.add new_winner value updated_map

let part_2 input seconds = let reindeers = process_input input in
                           let rec get_winners cur_seconds winners = if cur_seconds > seconds then
                                                                       winners
                                                                     else
                                                                       let (cur_winner, _) = get_current_winner reindeers cur_seconds in
                                                                       get_winners (cur_seconds + 1) (update_winners cur_winner winners) in
                           get_winners 1 (StringMap.empty)
                           |> StringMap.bindings
                           |> List.fold_left (fun (cur_name, cur_points) (name, points) -> if points > cur_points then (name, points) else (cur_name, cur_points)) ("", 0)
