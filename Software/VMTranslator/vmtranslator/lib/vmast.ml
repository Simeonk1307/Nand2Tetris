open Hackast

type segment = Argument | Local | Static | This | That | Pointer | Temp | Constant
type operation = Add | Sub | And | Or | Eq | Lt | Gt | Neg | Not

type vm_statement =
  | Push of     segment * int (* segment * index *)
  | Pop of      segment * int (* segment * index *)
  | Operator of operation

  | Label of    string (* label : file.func$lbl *)
  | Goto of     string (* label : file.func$lbl *)
  | Ifgoto of   string (* label : file.func$lbl *)

  | Function of string * int (* funcName * nVars*)
  | Return
  | Call of     string * int (* funcName * nArgs*)

type vm_program = vm_statement list
type asm_program = asm_expression list

let strseg_to_vmseg (seg : string) : segment =
  match seg with
  | "argument" ->   Argument
  | "local"    ->   Local
  | "static"   ->   Static
  | "this"     ->   This
  | "that"     ->   That
  | "pointer"  ->   Pointer
  | "temp"     ->   Temp
  | "constant" ->   Constant
  | _ -> failwith (__FUNCTION__ ^ ": only works for VM segment strings")

let seg_to_lbl (seg: segment) : string = 
  match seg with
  | Argument ->   "ARG" 
  | Local    ->   "LCL" 
  | This     ->   "THIS" 
  | That     ->   "THAT"
  | _ -> failwith (__FUNCTION__ ^ ": Unsupported segment type for conversion to Hack Asm predefined")


