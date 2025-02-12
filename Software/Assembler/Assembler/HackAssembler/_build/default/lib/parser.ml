open Ast
open Machine

(* Removes comments that start with "//", but keeps "/" characters in non-comment syntax *)
let remcomments (line: string) =
  try
    let comment_start = String.index line '/' in
    if comment_start < String.length line - 1 && String.get line (comment_start + 1) = '/' then
      String.sub line 0 comment_start
    else
      line
  with Not_found -> line

(* Parses and processes each line of code *)
let parse (line: string) (symbol_table: (string, int) Hashtbl.t ref) (line_count: int) : (asm_expression * int) =
  let trimmed_line = String.trim line in
  if String.starts_with ~prefix:"@" trimmed_line then
    let a_expr = string_to_AInstr (ref trimmed_line) in
    match a_expr with
    | At a -> (At a, line_count + 1)
    | VorL s -> (VorL s, line_count + 1)
  else if String.starts_with ~prefix:"(" trimmed_line && String.ends_with ~suffix:")" trimmed_line then
    let label = String.sub trimmed_line 1 (String.length trimmed_line - 2) in
    let label_expr = string_to_Label (ref trimmed_line) in
    if not (Hashtbl.mem !symbol_table label) then
      Hashtbl.add !symbol_table label line_count;
    (label_expr, line_count)
  else
    let c_expr = string_to_CInstr (ref trimmed_line) in
    (c_expr, line_count + 1)

let rec first_pass (instrref: asm_expression list ref) (symbol_table: (string, int) Hashtbl.t ref) (line_count: int) (reset: bool) =
  try
    if reset then (
      Hashtbl.reset !symbol_table;
      symbol_table := predefined_in_table symbol_table;
    );
    let line = String.trim (remcomments (read_line ())) in
    if line <> "" then (
      let instruction, linecount = parse line symbol_table line_count in
      match instruction with
      | Bracket _ -> first_pass instrref symbol_table linecount false  (* Ignore label definition *)
      | _ -> instrref := instruction :: !instrref; first_pass instrref symbol_table linecount false
    ) else (
      first_pass instrref symbol_table line_count false  (* Skip empty lines *)
    )
  with
  | End_of_file -> (List.rev !instrref, !symbol_table)

let second_pass result symbols var_count =
    List.map (fun (instr: asm_expression) ->
        match instr with
        | VorL s ->
          if Hashtbl.mem symbols s then
            encode (At (Hashtbl.find symbols s ))
          else begin
            Hashtbl.add symbols s !var_count;
            var_count := !var_count + 1;
            encode (At (Hashtbl.find symbols s ))
          end
        | _ -> encode instr
    ) result

(* Finalizes and encodes instructions, adjusting for variables and labels *)
let finale (instrref: asm_expression list ref) (symbol_table: (string, int) Hashtbl.t ref) (var_count: int ref) 
           (line_count: int) (reset: bool) =
  let (result, symbols) = first_pass instrref symbol_table line_count reset in
  let instructions = second_pass result symbols var_count in
  List.iter (fun encoded_instr -> Printf.printf "%s\n" encoded_instr) instructions