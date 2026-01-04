(*** Utilities ***)
let read_file filename = In_channel.with_open_text filename In_channel.input_all

type scene = {
    sue_number: int;
    children: int option;
    cats: int option;
    samoyeds: int option;
    pomeranians: int option;
    akitas: int option;
    viszlas: int option;
    goldfish: int option;
    trees: int option;
    cars: int option;
    perfumes: int option;
  }

let get_empty_scene sue_number = {
    sue_number = sue_number;
    children = None;
    cats = None;
    samoyeds = None;
    pomeranians = None;
    akitas = None;
    viszlas = None;
    goldfish = None;
    trees = None;
    cars = None;
    perfumes = None;
  }

let get_base_scene () = {
    sue_number = 0;
    children = Some 3;
    cats = Some 7;
    samoyeds = Some 2;
    pomeranians = Some 3;
    akitas = Some 0;
    viszlas = Some 0;
    goldfish = Some 5;
    trees = Some 3;
    cars = Some 2;
    perfumes = Some 1;
  }

(* A must be the base and is assumed to always be Some *)
let match_options a b = if Option.is_none b then
                          true
                        else
                          a = b

let match_scenes base scene = match_options base.children scene.children &&
                                match_options base.cats scene.cats &&
                                  match_options base.samoyeds scene.samoyeds &&
                                    match_options base.pomeranians scene.pomeranians &&
                                      match_options base.akitas scene.akitas &&
                                        match_options base.viszlas scene.viszlas &&
                                          match_options base.goldfish scene.goldfish &&
                                            match_options base.trees scene.trees &&
                                              match_options base.cars scene.cars &&
                                                match_options base.perfumes scene.perfumes

(*** Part 1 ***)
let parse_input str = let parse_line line = match List.map (fun x -> if String.ends_with ~suffix:":" x || String.ends_with ~suffix:"," x then String.sub x 0 ((String.length x) - 1) else x) (String.split_on_char ' ' line) with
                        | "Sue" :: num :: rest -> let base_sue = get_empty_scene (int_of_string num) in
                                                  let rec parse_line_inner properties scene = match properties with
                                                    | prop :: value :: others -> let opt = Some (int_of_string value) in
                                                                                       let new_scene = (match prop with
                                                                                                       | "children" -> { scene with children = opt; }
                                                                                                       | "cats" -> { scene with cats = opt; }
                                                                                                       | "samoyeds" -> { scene with samoyeds = opt; }
                                                                                                       | "pomeranians" -> { scene with pomeranians = opt; }
                                                                                                       | "akitas" -> { scene with akitas = opt; }
                                                                                                       | "vizslas" -> { scene with viszlas = opt; }
                                                                                                       | "goldfish" -> { scene with goldfish = opt; }
                                                                                                       | "trees" -> { scene with trees = opt; }
                                                                                                       | "cars" -> { scene with cars = opt; }
                                                                                                       | "perfumes" -> { scene with perfumes = opt }
                                                                                                       | a -> raise (Invalid_argument ("unrecognized property " ^ a))) in
                                                                                       parse_line_inner others new_scene
                                                    | [] -> scene
                                                    | _ -> raise (Invalid_argument "properties must be a key-value pair") in
                                                  parse_line_inner rest base_sue
                        | _ -> raise (Invalid_argument "unrecognized input format") in
                      String.split_on_char '\n' str
                      |> List.filter (fun x -> x <> "")
                      |> List.map parse_line

let part_1 str = let base = get_base_scene () in
                 parse_input str
                 |> List.filter (fun x -> match_scenes base x)

(*** Part 2 ***)
let match_options_compare a b op = if Option.is_none b then
                                  true
                                else
                                  let val_a = Option.get a in
                                  let val_b = Option.get b in
                                  op val_a val_b

let match_scenes_part_2 base scene = (match_options_compare base.cats scene.cats ( < )) &&
                                       (match_options_compare base.trees scene.trees ( < )) &&
                                         (match_options_compare base.pomeranians scene.pomeranians ( > )) &&
                                           (match_options_compare base.goldfish scene.goldfish ( > )) &&
                                             match_options base.children scene.children &&
                                               match_options base.samoyeds scene.samoyeds &&
                                                 match_options base.akitas scene.akitas &&
                                                   match_options base.viszlas scene.viszlas &&
                                                     match_options base.cars scene.cars &&
                                                       match_options base.perfumes scene.perfumes

let part_2 str = let base = get_base_scene () in
                 parse_input str
                 |> List.filter (fun x -> match_scenes_part_2 base x)
