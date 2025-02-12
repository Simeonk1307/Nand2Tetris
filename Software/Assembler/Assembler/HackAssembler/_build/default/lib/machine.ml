open Ast

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