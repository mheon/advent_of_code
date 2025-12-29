open Cryptokit

(*** Utilities ***)
let md5_hash msg = hash_string (Hash.md5 ()) msg

let bytes_to_list hash = List.init (Bytes.length hash) (Bytes.get hash)

let hash_to_hex hash = let as_bytes = bytes_to_list (String.to_bytes hash) in
                       List.map (fun x -> Printf.sprintf "%02x" (int_of_char x)) as_bytes
                       |> List.fold_left ( ^ ) ""

(*** Part 1 ***)
let md5_hash_with_key key i = md5_hash (key ^ Int.to_string i)

let rec md5_hash_until_solution key i to_check = let hash_bytes = md5_hash_with_key key i in
                                        let hash = hash_to_hex hash_bytes in
                                        let substring = String.sub hash 0 (String.length to_check) in
                                        if substring = to_check then
                                          i
                                        else
                                          md5_hash_until_solution key (i + 1) to_check

let find_solution_five_zeroes key = md5_hash_until_solution key 1 "00000"

(*** Part 2 ***)
let find_solution_six_zeroes key = md5_hash_until_solution key 1 "000000"
