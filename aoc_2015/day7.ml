(*** Utilities ***)

type operation =
  | Constant
  | And
  | Or
  | Not
  | Lshift of int
  | Rshift of int

type gate = {
    opcode: operation;
    in_1: string;
    in_2: string;
    output: string;
    value: int option;
  }

type value =
  | Const of int
  | Gate of gate
  | Empty

let any_option_none in1 in2 = Option.is_none in1 || Option.is_none in2

module StringMap = Map.Make(String)

let map_to_keys map = List.map (fun (k, _) -> k) (StringMap.bindings map)

let make_gate_map: gate StringMap.t = StringMap.empty

let set_gate target all_gates = StringMap.add target.output target all_gates

let update_gate target all_gates = StringMap.remove target.output all_gates
                                 |> set_gate target

let get_gate signal all_gates = match (StringMap.find_opt signal all_gates) with
  | Some g -> g
  | None -> raise (Invalid_argument ("no gate associated with signal " ^ signal))

let get_value_from_input input all_gates = if input = "" then
                                             Empty
                                           else match int_of_string_opt input with
                                                | Some s -> Const s
                                                | None -> Gate (get_gate input all_gates)

let mask = 0xFFFF

(*** Part 1 ***)

let make_gate_from_str str = let split_str = String.split_on_char ' ' str in
                             match split_str with
                             | const :: "->" :: out :: [] ->
                                {
                                  opcode = Constant;
                                  in_1 = const;
                                  in_2 = "";
                                  output = out;
                                  value = None;
                                }
                             | in1 :: "AND" :: in2 :: "->" :: out :: [] ->
                                {
                                  opcode = And;
                                  in_1 = in1;
                                  in_2 = in2;
                                  output = out;
                                  value = None;
                                }
                             | in1 :: "OR" :: in2 :: "->" :: out :: [] ->
                                {
                                  opcode = Or;
                                  in_1 = in1;
                                  in_2 = in2;
                                  output = out;
                                  value = None;
                                }
                             | input :: "LSHIFT" :: const :: "->" :: out :: [] ->
                                let const_int = int_of_string const in
                                {
                                  opcode = (Lshift const_int);
                                  in_1 = input;
                                  in_2 = "";
                                  output = out;
                                  value = None;
                                }
                             | input :: "RSHIFT" :: const :: "->" :: out :: [] ->
                                let const_int = int_of_string const in
                                {
                                  opcode = (Rshift const_int);
                                  in_1 = input;
                                  in_2 = "";
                                  output = out;
                                  value = None;
                                }
                             | "NOT" :: in1 :: "->" :: out :: [] ->
                                {
                                  opcode = Not;
                                  in_1 = in1;
                                  in_2 = "";
                                  output = out;
                                  value = None;
                                }
                             | _ -> raise (Invalid_argument "not a valid gate format")

let rec get_all_gates lines gates = match lines with
  | line :: rest -> let gate = make_gate_from_str line in
                    let new_gates = set_gate gate gates in
                    get_all_gates rest new_gates
  | [] -> gates

let logical_operation op in1 in2 = let val1 = Option.value in1 ~default:0 in
                                   let val2 = Option.value in2 ~default:0 in
                                   match op with
                                   | Constant -> if Option.is_none in1 then
                                                     None
                                                   else
                                                     Some (Int.logand val1 mask)
                                   | And -> if any_option_none in1 in2 then
                                              None
                                            else
                                              Some (Int.logand (Int.logand val1 val2) mask)
                                   | Or -> if any_option_none in1 in2 then
                                             None
                                           else
                                             Some (Int.logand (Int.logor val1 val2) mask)
                                   | Lshift c -> if Option.is_none in1 then
                                                   None
                                                 else
                                                   Some (Int.logand (Int.shift_left val1 c) mask)
                                   | Rshift c -> if Option.is_none in1 then
                                                   None
                                                 else
                                                   Some (Int.logand (Int.shift_right_logical val1 c) mask)
                                   | Not -> if Option.is_none in1 then
                                              None
                                            else
                                              Some (Int.logand (Int.lognot val1) mask)

let rec find_gate_value target all_gates = if Option.is_some target.value then
                                             (target, all_gates)
                                           else
                                             let (in1_value, updated_gates_1) = match get_value_from_input target.in_1 all_gates with
                                               | Empty -> (None, all_gates)
                                               | Const c -> (Some c, all_gates)
                                               | Gate g -> let (g_with_val, internal_gates) = find_gate_value g all_gates in
                                                           (g_with_val.value, internal_gates) in
                                             let (in2_value, updated_gates_2) = match get_value_from_input target.in_2 all_gates with
                                               | Empty -> (None, updated_gates_1)
                                               | Const c -> (Some c, updated_gates_1)
                                               | Gate g -> let (g_with_val, gates_2) = find_gate_value g updated_gates_1 in
                                                           (g_with_val.value, gates_2) in
                                             let final_value = logical_operation target.opcode in1_value in2_value in
                                             let final_gate =
                                               {
                                                 opcode = target.opcode;
                                                 in_1 = target.in_1;
                                                 in_2 = target.in_2;
                                                 output = target.output;
                                                 value = final_value;
                                               } in
                                             let return_gates = update_gate final_gate updated_gates_2 in
                                             (final_gate, return_gates)

let rec get_all_gate_values_inner keys all_gates = match keys with
  | k :: rest -> let gate = get_gate k all_gates in
                 let (_, new_gates) = find_gate_value gate all_gates in
                 get_all_gate_values_inner rest new_gates
  | [] -> all_gates

let get_all_gate_values all_gates = let signal_names = map_to_keys all_gates in
                                   get_all_gate_values_inner signal_names all_gates

let process_input input = let split_input = String.split_on_char '\n' input in
                          let gate_a = make_gate_map
                                       |> get_all_gates split_input
                                       |> get_all_gate_values
                                       |> get_gate "a" in
                          gate_a.value

(*** Part 2 ***)
let process_input_part_2 input = let split_input = String.split_on_char '\n' input in
                                 let gates_premod = make_gate_map
                                                    |> get_all_gates split_input in
                                 let modified_b =
                                   {
                                     opcode = Constant;
                                     in_1 = "46065";
                                     in_2 = "";
                                     output = "b";
                                     value = None;
                                   } in
                                 let modified_gates = update_gate modified_b gates_premod in
                                 let new_gate_a = get_all_gate_values modified_gates
                                                  |> get_gate "a" in
                                 new_gate_a.value
