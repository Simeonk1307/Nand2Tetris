open Hackast

let padding (s: string) : string = 
  let l = String.length s in
  let pad = String.make (16 - l) '0' in
  pad ^ s

let dectobin (n: int) : string =
    let rec to_binary n =
      if n = 0 then ""
      else to_binary (n / 2) ^ string_of_int (n mod 2)
    in
    if n = 0 then "0" else to_binary n

let mask7FFF (addr:int) = Int.logand addr 0x7FFF (*0111  1111 1111 1111 *)


let encodeA (k:a) : int = (*exhaustive*)
match k with
| Zero -> 0 | One -> 1
  
let encodeJUMP (j:jump) : int = (*exhaustive*)
match j with
| JNULL -> 0 | JGT -> 1 | JEQ -> 2 | JGE -> 3
| JLT -> 4   | JNE -> 5 | JLE -> 6 | JMP -> 7

let encodeDEST (d:destination) : int = (*non-exhaustive*)
match d with
| [] -> 0     | [M] -> 1 | [D] -> 2
| [D; M] -> 3 | [A] -> 4 | [A; M] -> 5
| [A; D] -> 6 | [A; D; M] -> 7
| _ -> failwith "Invalid destination list in machine.ml"

let encodeCOMP (c:computation) : string = (*exhaustive*)
match c with
| Const Zero -> "0b101010" | Const One ->  "0b111111" | Const MinusOne -> "0b111010"

| UApply (Id,D) ->  "0b001100"  | UApply (Id,A) ->  "0b110000"  | UApply (Id,M) ->  "0b110000"
| UApply (Not,D) ->  "0b001101" | UApply (Not,A) ->  "0b110001" | UApply (Not,M) ->  "0b110001"
| UApply (Neg,D) ->  "0b001111" | UApply (Neg,A) ->  "0b110011" | UApply (Neg,M) ->  "0b110011"
| UApply (Inc,D) ->  "0b011111" | UApply (Inc,A) ->  "0b110111" | UApply (Inc,M) ->  "0b110111"
| UApply (Dec,D) ->  "0b001110" | UApply (Dec,A) ->  "0b110010" | UApply (Dec,M) ->  "0b110010"

| BApply (DPlus,A) -> "0b000010"    | BApply (DPlus,M) ->  "0b000010"
| BApply (DMinus,A) -> "0b010011"   | BApply (DMinus,M) -> "0b010011"
| BApply (MAMinus,A) -> "0b000111"  | BApply (MAMinus,M) -> "0b000111"
| BApply (DAnd,A) -> "0b000000"     | BApply (DAnd,M) -> "0b000000"
| BApply (DOr,A) ->  "0b010101"     | BApply (DOr,M) ->  "0b010101"

let constructC ((k:a),(c:computation),(d:destination),(j:jump)) = (encodeJUMP j) + (Int.shift_left (encodeDEST d) 3) + (Int.shift_left (int_of_string (encodeCOMP c)) 6) + (Int.shift_left (encodeA k) 12) + (Int.shift_left 7 13)

let encode (exp: asm_expression) = 
match exp with
| At i -> padding (dectobin (mask7FFF i ))
| C  (k,c,d,j) -> padding (dectobin (constructC (k,c,d,j)))
| _ -> failwith "Unexpected Error in encode in machine.ml"



let first_pass_v2 (instructions: asm_expression list) (symbol_table: (string, int) Hashtbl.t ref) (line_count: int ref) : asm_expression list =
  List.fold_left (fun acc instr ->
      match instr with
      | Bracket label ->
          if not (Hashtbl.mem !symbol_table label) then
            Hashtbl.add !symbol_table label !line_count;
          acc  (* Ignore labels for the final instruction list *)
      | _ ->
          line_count := !line_count + 1;
          instr :: acc  (* Add actual instructions to the final list *)
    ) [] instructions
  |> List.rev  (* Reverse the accumulated list to maintain order *)




let second_pass_v2 (instructions: asm_expression list) (symbol_table: (string, int) Hashtbl.t ref) (var_count: int ref) : string list =
  List.map (fun instr ->
      match instr with
      | VorL s ->
          if Hashtbl.mem !symbol_table s then
            encode (At (Hashtbl.find !symbol_table s))
          else begin
            Hashtbl.add !symbol_table s !var_count;
            var_count := !var_count + 1;
            encode (At (Hashtbl.find !symbol_table s))
          end
      | _ -> encode instr
    ) instructions


let asm_to_machine_code (inp: asm_expression list) : string list =
  let symbol_table = ref (predefined_in_table (ref (Hashtbl.create 50))) in
  let line_count = ref 0 in
  let var_count = ref 16 in
  let first_pass_result = first_pass_v2 inp symbol_table line_count in
  second_pass_v2 first_pass_result symbol_table var_count









let reg_to_str (r: register) : string =
  match r with
  | A -> "A"
  | D -> "D"
  | M -> "M"

let jump_to_str (j:jump) : string =
  match j with
  | JNULL -> "" | JGT -> "JGT" | JEQ -> "JEQ" | JGE -> "JGE"
  | JLT -> "JLT"   | JNE -> "JNE" | JLE -> "JLE" | JMP -> "JMP"

let rec dest_to_str (d:destination) : string =
  match d with
  | [] -> ""
  | head :: tail -> (reg_to_str head) ^ (dest_to_str tail)

let comp_to_str (c: computation) : string =
  match c with
  | Const Zero -> "0" | Const One -> "1" | Const MinusOne -> "-1"
  | UApply (Id, D) -> "D" | UApply (Id, A) -> "A" | UApply (Id, M) -> "M"
  | UApply (Not, D) -> "!D" | UApply (Not, A) -> "!A" | UApply (Not, M) -> "!M"
  | UApply (Neg, D) -> "-D" | UApply (Neg, A) -> "-A" | UApply (Neg, M) -> "-M"
  | UApply (Inc, D) -> "D+1" | UApply (Inc, A) -> "A+1" | UApply (Inc, M) -> "M+1"
  | UApply (Dec, D) -> "D-1" | UApply (Dec, A) -> "A-1" | UApply (Dec, M) -> "M-1"
  | BApply (DPlus, A) -> "D+A" | BApply (DPlus, M) -> "D+M"
  | BApply (DMinus, A) -> "D-A" | BApply (DMinus, M) -> "D-M"
  | BApply (MAMinus, A) -> "A-D" | BApply (MAMinus, M) -> "M-D"
  | BApply (DAnd, A) -> "D&A" | BApply (DAnd, M) -> "D&M"
  | BApply (DOr, A) -> "D|A" | BApply (DOr, M) -> "D|M"


let asmsmall_to_str (inp: asm_expression) : string =
  match inp with
  | At i -> "At " ^ (string_of_int i)
  | VorL i -> "VorL " ^ i
  | Bracket i -> "Bracket " ^ i
  | C (_, c, d, j) -> (dest_to_str d) ^ "=" ^ (comp_to_str c) ^ ";" ^ (jump_to_str j)

let rec asm_to_str (inp:asm_expression list) : string list =
  match inp with
  | [] -> []
  | head :: tail -> (asmsmall_to_str head) :: (asm_to_str tail)