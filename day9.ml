(*** Utilities ***)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type edge = {
    loc1: string;
    loc2: string;
    weight: int;
  }

type node = {
    name: string;
    edges: edge list;
  }

type path = {
    cost: int;
    nodes: string list;
  }

let print_path path = print_string ("Path of cost " ^ (string_of_int path.cost) ^ " with stops ");
                      let rec print_stops stops = match stops with
                        | stop :: rest -> print_string (stop ^ " ");
                                          print_stops rest
                        | [] -> print_string "\n" in
                      print_stops path.nodes

let get_empty_graph () = StringMap.of_list []

let add_edge_to_node name node edge = let cur_edges = match node with
                                        | Some n -> n.edges
                                        | None -> [] in
                                      {
                                        name = name;
                                        edges = edge :: cur_edges;
                                      }

let update_node_in_graph new_node old_node graph = let updated_graph = match old_node with
                                                     | Some n -> StringMap.remove n.name graph
                                                     | None -> graph in
                                                   StringMap.add new_node.name new_node updated_graph

let add_edge_to_graph a b weight graph = let new_edge = {
                                             loc1 = a;
                                             loc2 = b;
                                             weight = weight;
                                           } in
                                         let node_a_opt = StringMap.find_opt a graph in
                                         let node_b_opt = StringMap.find_opt b graph in
                                         let new_a = add_edge_to_node a node_a_opt new_edge in
                                         let new_b = add_edge_to_node b node_b_opt new_edge in
                                         update_node_in_graph new_a node_a_opt graph
                                         |> update_node_in_graph new_b node_b_opt

let get_other_node edge cur_node = if edge.loc1 = cur_node then
                                     edge.loc2
                                   else
                                     edge.loc1

(*** Part 1 ***)
let rec find_optimal_path paths current compare_fn = match paths with
  | path :: rest -> (match current with
                    | None -> find_optimal_path rest (Some path) compare_fn
                    | Some c -> if (compare_fn path.cost c.cost) then
                                  find_optimal_path rest (Some path) compare_fn
                                else
                                  find_optimal_path rest current compare_fn
                    )
  | [] -> current

let rec build_graph_from_input lines graph = let parse_one_line line graph =
                                               let split_line = String.split_on_char ' ' line in
                                               match split_line with
                                               | nodeA :: "to" :: nodeB :: "=" :: weight :: [] ->
                                                  let int_weight = int_of_string weight in
                                                  add_edge_to_graph nodeA nodeB int_weight graph
                                               | _ -> raise (Invalid_argument ("bad node format " ^ line)) in
                                             match lines with
                                             | line :: rest -> parse_one_line line graph
                                                               |> build_graph_from_input rest
                                             | [] -> graph

let rec bfs current path graph remaining compare_fn = if StringSet.is_empty remaining then
                                             path
                                           else
                                             let current_node = StringMap.find current graph in
                                             let valid_edges = List.filter (fun x -> StringSet.mem (get_other_node x current) remaining) current_node.edges in
                                             let rec get_potential_paths edges = match edges with
                                               | edge :: rest -> let next_node = get_other_node edge current in
                                                                 let new_path = {
                                                                     cost = path.cost + edge.weight;
                                                                     nodes = next_node :: path.nodes;
                                                                   } in
                                                                 bfs next_node new_path graph (StringSet.remove next_node remaining) compare_fn :: get_potential_paths rest
                                               | [] -> [] in
                                             let new_paths = get_potential_paths valid_edges in
                                             Option.get (find_optimal_path new_paths None compare_fn)

let breadth_first_search compare_fn graph = let possible_starts = StringMap.bindings graph
                                                 |> List.map (fun (x, _) -> x) in
                                 let rec try_all_starts members = match members with
                                   | start :: rest -> let remaining = StringMap.bindings graph
                                                                      |> List.map (fun (x, _) -> x)
                                                                      |> List.filter (fun x -> x <> start)
                                                                      |> StringSet.of_list in
                                                      let path = {
                                                          cost = 0;
                                                          nodes = [start;];
                                                        } in
                                                      (bfs start path graph remaining compare_fn) :: try_all_starts rest
                                   | [] -> [] in
                                 find_optimal_path (try_all_starts possible_starts) None compare_fn

let get_best_path_weight input = let best_path = get_empty_graph ()
                                                 |> build_graph_from_input (String.split_on_char '\n' input)
                                                 |> breadth_first_search (fun x y -> x < y) in
                                 match best_path with
                                 | Some p -> print_path p
                                 | None -> raise (Invalid_argument "could not find a valid path")

(*** Part 2 ***)
let get_worst_path_weight input = let worst_path = get_empty_graph ()
                                                   |> build_graph_from_input (String.split_on_char '\n' input)
                                                   |> breadth_first_search (fun x y -> x > y) in
                                  match worst_path with
                                  | Some p -> print_path p
                                  | None -> raise (Invalid_argument "could not find a valid path")
