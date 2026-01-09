module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let get_factors n = let factors = ref IntSet.empty in
                    let max = int_of_float @@ Float.sqrt @@ float_of_int n in
                    for i = 1 to max do
                      if n mod i = 0 then
                        factors := IntSet.add i @@ IntSet.add (n / i) !factors
                    done;
                    IntSet.to_list !factors

(*** Part 1 ***)
let calculate_score_for_stop n = get_factors n
                                 |> List.map (fun n -> n * 10)
                                 |> List.fold_left (+) 0

let rec try_stops_until_score stop score = if calculate_score_for_stop stop >= score then
                                             stop
                                           else
                                             try_stops_until_score (stop + 1) score

(*** Part 2 ***)
let rec update_exclude_count factors exclude_count = match factors with
  | [] -> exclude_count
  | f :: rest -> let new_map = match IntMap.find_opt f exclude_count with
                   | None -> IntMap.add f 1 exclude_count
                   | Some count -> if count < 50 then
                                     let updated_map = IntMap.remove f exclude_count in
                                     IntMap.add f (count + 1) updated_map
                                   else
                                     exclude_count in
                 update_exclude_count rest new_map

let calculate_score_for_stop_part_2 n exclude_count = let factors = get_factors n in
                                                      let new_excluded = update_exclude_count factors exclude_count in
                                                      let score = List.filter (fun f -> match IntMap.find_opt f exclude_count with
                                                                                        | Some s -> if s >= 50 then false else true
                                                                                        | None -> true) factors
                                                                  |> List.map (fun n -> n * 11)
                                                                  |> List.fold_left (+) 0 in
                                                      (score, new_excluded)

let try_stops_until_score_part_2 score = let rec try_stops_inner stop exclude_count = let (test_score, new_exclude) = calculate_score_for_stop_part_2 stop exclude_count in
                                                                                      if test_score >= score then
                                                                                        stop
                                                                                      else
                                                                                        try_stops_inner (stop + 1) new_exclude in
                                         try_stops_inner 1 IntMap.empty
