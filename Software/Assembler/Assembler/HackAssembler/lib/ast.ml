(* Type Definitions Start *)
type register = A | D | M
type aORmreg = A | M

type destination = register list
type a = Zero | One

type constants = Zero | One | MinusOne
type unary = Id | Not | Neg | Inc | Dec
type binary = DPlus | DMinus | MAMinus | DAnd | DOr

type computation = | Const of constants | UApply of unary * register | BApply of binary * aORmreg

type jump = JNULL | JGT | JEQ | JGE | JLT | JNE | JLE | JMP

type asm_expression = 
  | At of int | VorL of string | Bracket of string
  | C of a * computation * destination * jump 

type a_instr_type =
  | At of int
  | VorL of string
(* Type Definitions End *)

(* Helper functions Start **)
let remwhitev1 line = 
  String.concat "" (String.split_on_char '\t' (String.concat "" (String.split_on_char ' ' line)))

let print_hash_strint1 (ht: (string, int) Hashtbl.t) =
  Hashtbl.iter (fun key value ->
    Printf.printf "Key: %s, Value: %d\n" key value
  ) ht

let print_hash_strint_ascending (ht: (string, int) Hashtbl.t) =
    let kv_list = Hashtbl.fold (fun key value acc -> (key, value) :: acc) ht [] in
    let sorted_list = List.sort (fun (_, v1) (_, v2) -> compare v1 v2) kv_list in
    List.iter (fun (key, value) -> Printf.printf "Key: %s, Value: %d\n" key value) sorted_list
  
let predefined_in_table (symbol_table: (string, int) Hashtbl.t ref) : (string, int) Hashtbl.t =
    let predefined_symbols = [
      ("R0", 0); ("R1", 1); ("R2", 2); ("R3", 3); ("R4", 4);
      ("R5", 5); ("R6", 6); ("R7", 7); ("R8", 8); ("R9", 9);
      ("R10", 10); ("R11", 11); ("R12", 12); ("R13", 13);
      ("R14", 14); ("R15", 15); ("SCREEN", 16384); ("KBD", 24576);
      ("SP", 0); ("LCL", 1); ("ARG", 2); ("THIS", 3); ("THAT", 4)
    ] in
    List.iter (fun (symbol, address) -> Hashtbl.add !symbol_table symbol address) predefined_symbols;
    !symbol_table
(* Helper functions End **)

(* Basic type conversions Start **)
let int_to_a (i: int) : a =
  match i with
  | 0 -> Zero | 1 -> One
  | _ -> failwith "Invalid int_to_a conversion in assem.ml"

let str_to_dest (s: string) : destination = 
  match s with 
  | "" -> []
  | "A" -> [A] | "D" -> [D] | "M" -> [M]
  | "DM" | "MD" -> [D; M] | "AM" | "MA" -> [A; M] | "AD" | "DA" -> [A; D]
  | "ADM" | "AMD" | "DAM" | "DMA" | "MAD" | "MDA" -> [A; D; M]
  | _ -> failwith "Invalid str_to_destination conversion in assem.ml"

let str_to_jump (s: string) : jump = 
  match s with 
  | "JNULL" -> JNULL | "JGT" -> JGT | "JEQ" -> JEQ | "JGE" -> JGE 
  | "JLT" -> JLT     | "JNE" -> JNE | "JLE" -> JLE | "JMP" -> JMP
  | _ -> failwith "Invalid str_to_jump conversion in assem.ml"

let str_to_computation (s: string) : computation = 
  match s with 
  | "1" -> Const One | "0" -> Const Zero | "-1" -> Const MinusOne
  | "D" -> UApply (Id, D) | "A" -> UApply (Id, A) | "M" -> UApply (Id, M)
  | "!D" -> UApply (Not, D) | "!A" -> UApply (Not, A) | "!M" -> UApply (Not, M)
  | "-D" -> UApply (Neg, D) | "-A" -> UApply (Neg, A) | "-M" -> UApply (Neg, M)
  | "D+1" | "1+D" -> UApply (Inc, D) | "A+1" | "1+A" -> UApply (Inc, A) | "M+1" | "1+M" -> UApply (Inc, M)
  | "D-1" -> UApply (Dec, D) | "A-1" -> UApply (Dec, A) | "M-1" -> UApply (Dec, M)
  | "D+A" | "A+D" -> BApply (DPlus, A) | "D+M" | "M+D" -> BApply (DPlus, M)
  | "D-A" -> BApply (DMinus, A) | "D-M" -> BApply (DMinus, M)
  | "A-D" -> BApply (MAMinus, A)| "M-D" -> BApply (MAMinus, M)
  | "D&A" | "A&D" -> BApply (DAnd, A) | "D&M" | "M&D" -> BApply (DAnd, M)
  | "D|A" | "A|D" -> BApply (DOr, A) | "D|M" | "M|D" -> BApply (DOr, M)
  | _ -> failwith "Invalid str_to_computation conversion in assem.ml"
(* Basic type conversions End **)

(* Compound type conversions Start **)
let string_to_AInstr (line: string ref) : a_instr_type =
  let trimmed_line = String.trim !line in
  if String.starts_with ~prefix:"@" trimmed_line then
    let straddr = String.trim (String.sub trimmed_line 1 (String.length trimmed_line - 1)) in
    let instruction =
      try Some (int_of_string straddr)
      with Failure _ -> None
    in
    match instruction with
    | Some intaddr -> At intaddr
    | None -> VorL straddr
  else
    failwith ("Invalid A-instruction format: " ^ !line)

let string_to_Label (line: string ref) : asm_expression =
  let trimmed_line = String.trim !line in
  if String.starts_with ~prefix:"(" trimmed_line && String.ends_with ~suffix:")" trimmed_line then
    let l = String.trim (String.sub trimmed_line 1 (String.length trimmed_line - 2)) in
    let pattern = "^[a-zA-Z_.$:][a-zA-Z0-9_.$:]*$" in
    let regex = Str.regexp pattern in
    if Str.string_match regex l 0 then
      Bracket l
    else
      failwith "Invalid Label Definition format in assem.ml"
  else
    failwith "Invalid Label Definition format in assem.ml"

let string_to_CInstr (line: string ref) : asm_expression =
  line := remwhitev1 !line;
  let d, j, c =
    let d =
      if String.contains !line '=' then
        let parts1 = String.split_on_char '=' !line in
        let dest = str_to_dest (List.nth parts1 0) in
        line := List.nth parts1 1;
        dest
      else []  
    in
    let j =
      if String.contains !line ';' then
        let parts2 = String.split_on_char ';' !line in
        let jump = str_to_jump (List.nth parts2 1) in
        line := List.nth parts2 0;
        jump
      else str_to_jump "JNULL"
    in
    let c = str_to_computation !line in
    (d, j, c)
  in
  let a = if String.contains !line 'M' then int_to_a 1 else int_to_a 0 in
  C (a, c, d, j)
(* Compound type conversions End **)