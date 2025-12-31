(*** Utilities ***)

let string_to_char_list in_str = (List.init (String.length in_str) (String.get in_str))

let read_file filename = In_channel.with_open_text filename In_channel.input_all

(*** Part 1 ***)
let rec get_code_memory_character_count str_list code_chars mem_chars = match str_list with
  | '\\' :: 'x' :: _ :: _ :: rest -> get_code_memory_character_count rest (code_chars + 4) (mem_chars + 1)
  | '\\' :: '\\' :: rest -> get_code_memory_character_count rest (code_chars + 2) (mem_chars + 1)
  | '\\' :: '"' :: rest -> get_code_memory_character_count rest (code_chars + 2) (mem_chars + 1)
  | '"' :: rest -> get_code_memory_character_count rest (code_chars + 1) mem_chars
  | _ :: rest -> get_code_memory_character_count rest (code_chars + 1) (mem_chars + 1)
  | [] -> (code_chars, mem_chars)

let rec process_all_lines lines code_chars mem_chars = match lines with
  | line :: rest -> let (added_code, added_mem) = get_code_memory_character_count (string_to_char_list line) 0 0 in
                    process_all_lines rest (code_chars + added_code) (mem_chars + added_mem)
  | [] -> (code_chars, mem_chars)

let process_input filename = let lines = read_file filename
                                         |> String.split_on_char '\n' in
                             let (code_chars, mem_chars) = process_all_lines lines 0 0 in
                             code_chars - mem_chars

(*** Part 2 ***)
let rec escape_string char_list = match char_list with
  | '"' :: rest -> '\\' :: '"' :: escape_string rest
  | '\\' :: rest -> '\\' :: '\\' :: escape_string rest
  | s :: rest -> s :: escape_string rest
  | [] -> []

let add_quotes char_list = '"' :: char_list @ ['"']

let rec increase_from_encoding char_list = let encoded_str = escape_string char_list
                                                             |> add_quotes in
                                           (List.length encoded_str) - (List.length char_list)

let rec process_all_lines_part_2 lines diff = match lines with
  (* Handle extra newlines... *)
  | line :: rest -> if line = "" then
                      process_all_lines_part_2 rest diff
                    else
                      let inc = increase_from_encoding (string_to_char_list line) in
                      process_all_lines_part_2 rest (diff + inc)
  | [] -> diff

let process_input_part_2 filename = let lines = read_file filename
                                                |> String.split_on_char '\n' in
                                    process_all_lines_part_2 lines 0

let rec count_backslash_quote line = match line with
  | c :: rest -> if c = '"' || c = '\\' then
                   1 + count_backslash_quote rest
                 else
                   count_backslash_quote rest
  | [] -> 0

let rec process_one_line line = let count = string_to_char_list line
                                            |> count_backslash_quote in
                                count + 2

let alternative_process_input_part_2 filename = read_file filename
                                                |> String.split_on_char '\n'
                                                |> List.map process_one_line
                                                |> List.fold_left ( + ) 0
