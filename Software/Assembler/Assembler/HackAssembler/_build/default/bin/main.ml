open Libr

let () = 
  let instructions = ref [] in
  let var_count = ref 16 in
  let line_count = 0 in
  let symbol_table = ref (Hashtbl.create 10000) in
  Parser.finale instructions symbol_table var_count line_count true;
  (* Ast.print_hash_strint_ascending !symbol_table; *)
