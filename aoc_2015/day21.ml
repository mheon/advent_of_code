type stats = {
    cost: int;
    damage: int;
    armor: int;
  }

let make_weapon cost damage = {
    cost = cost;
    damage = damage;
    armor = 0;
  }

let make_armor cost armor = {
    cost = cost;
    damage = 0;
    armor = armor;
  }

let weapons = [make_weapon 8 4; make_weapon 10 5; make_weapon 25 6; make_weapon 40 7; make_weapon 74 8]
let armor = [make_armor 0 0; make_armor 13 1; make_armor 31 2; make_armor 53 3; make_armor 75 4; make_armor 102 5]
let rings = [make_weapon 0 0; make_weapon 0 0; make_weapon 25 1; make_weapon 50 2; make_weapon 100 3; make_armor 20 1; make_armor 40 2; make_armor 80 3]

type winner = | Player
              | Boss

let rec simulate_combat (player_stats, player_hp) (boss_stats, boss_hp) =
  if boss_hp <= 0 then
    Player
  else if player_hp <= 0 then
    Boss
  else
    let attack atk_damage def_armor def_hp = let dmg = atk_damage - def_armor in
                                             if dmg <= 0 then
                                               def_hp - 1
                                             else
                                               def_hp - dmg in
    let simulate_one_turn player_hp boss_hp = let new_boss_hp = attack player_stats.damage boss_stats.armor boss_hp in
                                              let new_player_hp = attack boss_stats.damage player_stats.armor player_hp in
                                              (new_player_hp, new_boss_hp) in
    let (new_player_hp, new_boss_hp) = simulate_one_turn player_hp boss_hp in
    simulate_combat (player_stats, new_player_hp) (boss_stats, new_boss_hp)

let get_final_stats weapon armor (ring1, ring2) =
  {
    cost = weapon.cost + armor.cost + ring1.cost + ring2.cost;
    damage = weapon.damage + ring1.damage + ring2.damage;
    armor = armor.armor + ring1.armor + ring2.armor;
  }

let generate_ring_combinations = let rings_arr = Array.of_list rings in
                                 let max_iter = Array.length rings_arr - 1 in
                                 let combinations = ref [] in
                                 for i = 0 to max_iter do
                                   for j = 0 to max_iter do
                                     if i <> j then
                                       combinations := (Array.get rings_arr i, Array.get rings_arr j) :: !combinations
                                   done
                                 done;
                                 !combinations

let get_all_combinations = let ring_combinations = generate_ring_combinations in
                           let combinations = ref [] in
                           for w = 0 to List.length weapons - 1 do
                             for a = 0 to List.length armor - 1 do
                               for r = 0 to List.length ring_combinations - 1 do
                                 combinations := get_final_stats (List.nth weapons w) (List.nth armor a) (List.nth ring_combinations r) :: !combinations
                               done
                             done
                           done;
                           List.sort (fun x y -> compare x.cost y.cost) !combinations

let parse_input input = match List.flatten @@ List.map (fun x -> String.split_on_char ' ' x) @@ String.split_on_char '\n' input with
  | "Hit" :: "Points:" :: hp :: "Damage:" :: dmg :: "Armor:" :: armor :: [] ->
     ({
       cost = 0;
       damage = int_of_string dmg;
       armor = int_of_string armor;
       }, int_of_string hp)
  | _ -> raise (Invalid_argument "unrecognized input format")

let part_1 input = let boss_stats = parse_input input in
                   let ordered_combinations = get_all_combinations in
                   let rec try_all_combinations combinations = match combinations with
                     | c :: rest -> if simulate_combat (c, 100) boss_stats = Player then
                                      c.cost
                                    else
                                      try_all_combinations rest
                     | [] -> raise (Invalid_argument "no winning combination of weapons") in
                   try_all_combinations ordered_combinations

let part_2 input = let boss_stats = parse_input input in
                   let ordered_combinations = List.rev @@ get_all_combinations in
                   let rec try_all_combinations combinations = match combinations with
                     | c :: rest -> if simulate_combat (c, 100) boss_stats = Boss then
                                      c.cost
                                    else
                                      try_all_combinations rest
                     | [] -> raise (Invalid_argument "no winning combination of weapons") in
                   try_all_combinations ordered_combinations
