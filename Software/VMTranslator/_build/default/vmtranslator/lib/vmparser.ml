open Vmcodewriter
open Hackmachine

(* Removes comments that start with "//", but keeps "/" characters in non-comment syntax *)
let remcomments (line: string) =
  try
    let comment_start = String.index line '/' in
    if comment_start < String.length line - 1 && String.get line (comment_start + 1) = '/' then
      String.sub line 0 comment_start
    else
      line
  with Not_found -> line

let read_file filepath =
  let ic = open_in filepath in
  let fileandext = List.nth (List.rev (String.split_on_char '/' (String.trim filepath))) 0 in
  
  if String.ends_with ~suffix:".vm" fileandext then begin
    let filename = String.sub fileandext 0 ((String.length fileandext) - 3) in
    let fn       = ref "" in 
    let call_c   = ref 0  in
    let comp_c   = ref 0  in
    let final    = ref (Boot_Helper.bootstrap filename) in  
    try
      while true do
        let line = input_line ic in
        let stripped = String.trim (remcomments line) in
        let tokens = Str.split (Str.regexp " +") stripped in
        let length = List.length tokens in
          if length > 0 then begin
            if (List.hd tokens) = "function" then fn := List.nth tokens 1;
            if (List.hd tokens) = "call"     then call_c := !call_c + 1; 
            if (List.hd tokens) = "eq" || (List.hd tokens) = "lt" || (List.hd tokens) = "gt" then comp_c := !comp_c + 1;
            if !fn = "" then failwith "It must start with a function"
            else begin
            let instr = Token_to_Asm.translator tokens in
            final := !final @ (instr_to_asm filename !fn !call_c !comp_c instr); end
          end
      done;
    with End_of_file -> close_in ic;

    (* Encode final list and print results *)

    let encoded =  asm_to_machine_code !final in
      List.iter (fun i -> Printf.printf "%s\n" i) encoded end
      (* (asm_to_str !final) *)
  else failwith "File extension must be .vm" 
