open Hackast
open Vmast

module Boot_Helper = struct 
  let bootSP : asm_program = 
    [
      At 256; 
      string_to_CInstr (ref "D=A"); 
      VorL "SP"; 
      string_to_CInstr (ref "M=D"); 
    ]

  let syscall (lbl: string): asm_program = 
    [
      VorL lbl;
      string_to_CInstr (ref "0;JMP");
    ]

  let bootstrap (fileName:string): asm_program =
    let funcName = "Sys.init" in 
    let funclbl = (fileName ^ "." ^ funcName) in
    List.concat [
      bootSP;
      syscall funclbl;
    ]
end


module Push_Helper = struct 
  let push_to_stack : asm_program = [
    VorL "SP";
    string_to_CInstr (ref "A=M");
    string_to_CInstr (ref "M=D");
    VorL "SP";
    string_to_CInstr (ref "M=M+1");
  ]

  let argument (index: int) : asm_program = [
    VorL "ARG";
    string_to_CInstr (ref "D=M"); (* Seg *)
    At index; (* i *)
    string_to_CInstr (ref "A=D+A"); (* Seg + i *)
    string_to_CInstr (ref "D=M"); (* *(Seg + i) *)
  ]

  let local (index: int) : asm_program = [
    VorL "LCL";
    string_to_CInstr (ref "D=M"); (* Seg *)
    At index; (* i *)
    string_to_CInstr (ref "A=D+A"); (* Seg + i *)
    string_to_CInstr (ref "D=M"); (* *(Seg + i) *)
  ]

  let this (index: int) : asm_program = [
    VorL "THIS";
    string_to_CInstr (ref "D=M"); (* Seg *)
    At index; (* i *)
    string_to_CInstr (ref "A=D+A"); (* Seg + i *)
    string_to_CInstr (ref "D=M"); (* *(Seg + i) *)
  ]

  let that (index: int) : asm_program = [
    VorL "THAT";
    string_to_CInstr (ref "D=M"); 
    At index; 
    string_to_CInstr (ref "A=D+A"); (* Seg + i *)
    string_to_CInstr (ref "D=M"); (* *(Seg + i) *)
  ]
  
  let constant (index: int) : asm_program = 
    if index < 0 || index > 32768 then failwith  (__FUNCTION__ ^ ": Index out of range (0 to 32768)")
    else 
    [ At index; 
      string_to_CInstr (ref "D=A");
    ]
  
  let static (filename: string) (index: int) : asm_program =
    let label = filename ^"."^ (string_of_int index) 
    in
    [
      VorL label; 
      string_to_CInstr (ref "D=M");
    ]
  
  let pointer (index: int) : asm_program =
    if index < 0 || index > 1 then failwith (__FUNCTION__ ^ ": Index out of range (0 to 1)")
    else 
      [
        At index; 
        string_to_CInstr (ref "D=A"); 
        VorL "THIS";
        string_to_CInstr (ref "A=D+A"); 
        string_to_CInstr (ref "D=M");
      ] 

  let temp (index: int) : asm_program =
    if index < 0 || index > 7 then failwith (__FUNCTION__ ^ ": Index out of range (0 to 7)")
    else
      [
        At 5; 
        string_to_CInstr (ref "D=A"); 
        At index;
        string_to_CInstr (ref "A=D+A"); 
        string_to_CInstr (ref "D=M");
      ] 

  let push (filename: string) (seg: segment) (index: int) : asm_program =
    let segment_logic = match seg with
      | Static ->     static      filename    index 
      | Argument ->   argument    index
      | Local ->      local       index
      | This ->       this        index
      | That ->       that        index
      | Pointer ->    pointer     index
      | Temp ->       temp        index
      | Constant ->   constant    index

    in segment_logic @ push_to_stack
  
end

module Pop_Helper = struct 
  let pop_from_stack : asm_program = [
    VorL "R13";
    string_to_CInstr (ref "M=D");
    VorL "SP";
    string_to_CInstr (ref "AM=M-1");
    string_to_CInstr (ref "D=M");
    VorL "R13";
    string_to_CInstr (ref "A=M");
    string_to_CInstr (ref "M=D");
  ]

  let argument (index: int) : asm_program = [
    VorL "ARG";
    string_to_CInstr (ref "D=M"); (* Seg *)
    At index; (* i *)
    string_to_CInstr (ref "D=D+A"); 
  ] 

  let local (index: int) : asm_program = [
    VorL "LCL";
    string_to_CInstr (ref "D=M"); (* Seg *)
    At index; (* i *)
    string_to_CInstr (ref "D=D+A"); 
  ]

  let this (index: int) : asm_program = [
    VorL "THIS";
    string_to_CInstr (ref "D=M"); (* Seg *)
    At index; (* i *)
    string_to_CInstr (ref "D=D+A"); 
  ]

  let that (index: int) : asm_program = [
    VorL "THAT";
    string_to_CInstr (ref "D=M"); (* Seg *)
    At index; (* i *)
    string_to_CInstr (ref "D=D+A"); 
  ]
  
  let static (filename: string) (index: int) : asm_program =
    let label = filename ^"."^ (string_of_int index) 
    in
    [
      VorL label; 
      string_to_CInstr (ref "D=M")
    ]
  
  let pointer (index: int) : asm_program =
    if index < 0 || index > 1 then failwith (__FUNCTION__ ^ ": Index out of range (0 to 1)")
    else 
      [
        At index; 
        string_to_CInstr (ref "D=A"); 
        VorL "THIS";
        string_to_CInstr (ref "D=D+A");
      ] 

  let temp (index: int) : asm_program =
    if index < 0 || index > 7 then failwith (__FUNCTION__ ^ ": Index out of range (0 to 7)")
    else
      [
        At 5; 
        string_to_CInstr (ref "D=A"); 
        At index;
        string_to_CInstr (ref "D=D+A");
      ] 

  let pop (filename: string) (seg: segment) (index: int) : asm_program =
    let segment_logic = match seg with
      | Static ->     static      filename    index 
      | Argument ->   argument    index
      | Local ->      local       index
      | This ->       this        index
      | That ->       that        index
      | Pointer ->    pointer     index
      | Temp ->       temp        index

      | Constant ->   failwith (__FUNCTION__ ^ ": pop not allowed on constant segment")

    in segment_logic @ pop_from_stack
    
end



module Branch_Helper = struct 
  let goto (lbl: string) : asm_program =
    [ 
      VorL lbl;
      string_to_CInstr (ref "0;JMP");     (* unconditional jump *)
    ]
  
  let ifgoto (lbl: string) : asm_program =
    [ 
      VorL "SP";
      string_to_CInstr (ref "M=M-1");    (* pop() *)
      string_to_CInstr (ref "A=M");
      string_to_CInstr (ref "D=M");
      VorL lbl;
      string_to_CInstr (ref "D;JNE");     (* conditional jump *)
    ]
end

module Label_Helper = struct
  let label (lbl: string) : asm_program =
  [  Bracket lbl;  ]
end



module Operation_Helper = struct 
  let op_to_str (op: operation) : string = 
    match op with
      | Neg -> "-"   | Not -> "!" 
      | Add -> "+"   | Sub -> "-"            
      | And -> "&"   | Or -> "|" 
      | Eq -> "JEQ"  | Lt -> "JLT"  | Gt -> "JGT"

  let op_to_str2 (op:operation) : string =
    match op with
      | Neg -> "Neg" | Not -> "Not" 
      | Add -> "Add" | Sub -> "Sub" 
      | And -> "And" | Or -> "Or" 
      | Eq -> "Eq"   | Lt -> "Lt"   | Gt -> "Gt"

  let compare_opp_str (op: operation) : string = 
    match op with
      | Eq -> "JNE"  
      | Lt -> "JGE"  
      | Gt -> "JLE"
      | _ -> failwith (__FUNCTION__ ^ ": Only applicable for Eq, Lt, Gt")


  let unary (op: operation) : asm_program =
    match op with
    | Neg | Not -> begin
        let unary_op = (op_to_str op) in
        [
          VorL "SP";
          string_to_CInstr (ref "A=M-1"); (* X *)
          string_to_CInstr (ref ("M=" ^ unary_op ^ "M"));
        ]
      end
    | _ -> failwith (__FUNCTION__ ^ ": only allows unary operators")
  

  let arithmetic (op: operation) : asm_program =
  match op with
  | Add | Sub -> begin
      let binary_op = op_to_str op in
      [
          VorL "SP";
          string_to_CInstr (ref "M=M-1");
          string_to_CInstr (ref "A=M");
          string_to_CInstr (ref "D=M"); (* Y *)
          VorL "SP";
          string_to_CInstr (ref "A=M-1"); (* X *)
          string_to_CInstr (ref ("M=M" ^ binary_op ^ "D"));
        ]
    end
  | _ -> failwith (__FUNCTION__ ^ ": only allows binary arithmetic operators")

  let bitwise (op: operation) : asm_program =
    match op with
    | And | Or -> begin
        let binary_op = op_to_str op in
        [
          VorL "SP";
          string_to_CInstr (ref "M=M-1");
          string_to_CInstr (ref "A=M");
          string_to_CInstr (ref "D=M"); (* Y *)
          VorL "SP";
          string_to_CInstr (ref "A=M-1"); (* X *)
          string_to_CInstr (ref ("M=M" ^ binary_op ^ "D"));
        ]
      end
    | _ -> failwith (__FUNCTION__ ^ ": only allows binary bitwise operators")
  
  let createLabel (fileName: string) (funcName: string) (i:int) (ope: string) =
      let truth_label = fileName ^ "." ^ funcName ^ ".truthfulness." ^ ope ^ "." ^ (string_of_int i)in
      let falsity_label =  fileName ^ "." ^ funcName ^ ".falsity." ^ ope  ^ "." ^ (string_of_int i) in
      let continue_label = fileName ^ "." ^ funcName ^ ".continue." ^ ope  ^ "." ^ (string_of_int i) in
      (truth_label, falsity_label, continue_label)


  let comparison  (fileName:string) (funcName: string) (i: int) (op: operation) : asm_program =
    match op with
    | Eq | Lt | Gt ->
  
      let ope = op_to_str op in
      let complement = compare_opp_str op in 
      let ope2 = op_to_str2  op in
      let (truth_label, falsity_label, continue_label) = createLabel fileName funcName i ope2 in
      [
        VorL "SP";
        string_to_CInstr (ref "M=M-1");
        string_to_CInstr (ref "A=M");
        string_to_CInstr (ref "D=M"); (* Y *)
        VorL "SP";
        string_to_CInstr (ref "A=M-1"); (* X *)
        string_to_CInstr (ref "D=M-D"); (* x-y *)
  
        VorL truth_label;
          string_to_CInstr (ref ("D;" ^ ope ));
  
        VorL falsity_label;
          string_to_CInstr (ref ("D;" ^ complement ));
  
        Bracket truth_label;
          VorL "SP";
          string_to_CInstr (ref "A=M-1");
          string_to_CInstr (ref "M=-1");
          VorL continue_label;
           string_to_CInstr (ref ("0;JMP"));
  
        Bracket falsity_label;
          VorL "SP";
          string_to_CInstr (ref "A=M-1");
          string_to_CInstr (ref "M=0");
          VorL continue_label;
           string_to_CInstr (ref ("0;JMP"));

        Bracket continue_label;
      ]
    | _ -> failwith (__FUNCTION__ ^ ": only allows comparison operators")


  let oper (fileName: string) (funcName:string) (i: int) (op: operation) : asm_program =
  match op with
  | Neg | Not     -> unary        op
  | Add | Sub     -> arithmetic   op
  | And | Or      -> bitwise      op
  | Eq | Gt | Lt  -> comparison   fileName  funcName   i   op
end



module Call_Helper = struct
  let push_retAddr (returnlbl : string) : asm_program =
    [
      VorL returnlbl;
      string_to_CInstr (ref "D=A");
      VorL "SP";
      string_to_CInstr (ref "A=M");
      string_to_CInstr (ref "M=D"); (* SP = returnlbl *)
      VorL "SP";
      string_to_CInstr (ref "M=M+1"); (* SP++ *)
    ]

  let push_addr (seg : segment) : asm_program =
    [
      VorL (seg_to_lbl seg);
      string_to_CInstr (ref "D=M");
      VorL "SP";
      string_to_CInstr (ref "A=M");
      string_to_CInstr (ref "M=D"); (* SP = LCL | ARG | THIS | THAT *)
      VorL "SP";
      string_to_CInstr (ref "M=M+1"); (* SP++ *)
    ]

  let repositionARG (nArgs : int) : asm_program =
    if nArgs < 0 then 
      failwith "Number of arguments cannot be negative"
    else
    [
      VorL "SP";
      string_to_CInstr (ref "D=M"); (* D = SP *)
      At nArgs;
      string_to_CInstr (ref "D=D-A"); (* D = SP - nArgs *)
      At 5;
      string_to_CInstr (ref "D=D-A"); (* D = SP - nArgs - 5 *)
      VorL "ARG";
      string_to_CInstr (ref "M=D"); (* ARG = SP - nArgs - 5 *)
    ]

  let repositionLCL : asm_program =
    [
      VorL "SP";
      string_to_CInstr (ref "D=M"); (* D = SP *)
      VorL "LCL";
      string_to_CInstr (ref "M=D") (* LCL = SP *)
    ]

  
  let call (fileName:string) (funcName: string) (i : int) (nArgs : int)  : asm_program =
    let returnlbl = fileName ^ "." ^ funcName ^ "$ret." ^ (string_of_int i) in 
    let funclbl = fileName ^ "." ^ funcName in
    List.concat
      [
        push_retAddr          returnlbl;    (* Push return address *)
        push_addr             Local;        (* Push LCL *)
        push_addr             Argument;     (* Push ARG *) 
        push_addr             This;         (* Push THIS *)
        push_addr             That;         (* Push THAT *)
        repositionARG         nArgs;        (* Reposition ARG *)
        repositionLCL         ;             (* Reposition LCL *)
        Branch_Helper.goto    funclbl;      (* Unconditional Jump to function *)
        Label_Helper.label    returnlbl;    (* Mark return address *)
      ]

end

module Function_Helper = struct 
  let rec push_0n (n:int) : asm_program = 
    if n < 0 then failwith (__FUNCTION__ ^ ": integer must be non-negative")
    else begin
      if n = 0 then [] 
      else
        Push_Helper.constant 0 
        @ Push_Helper.push_to_stack 
        @ (push_0n (n - 1)) 
    end


  let preamble (fileName:string) (funcName: string) (nlocal: int) : asm_program =
    let funclbl = (fileName ^ "." ^ funcName) in
      List.concat [
          Label_Helper.label    funclbl;             (* Create function label *)
          push_0n               nlocal;              (* Initialize local variables to 0 *)
      ]
end


module Return_Helper = struct
  let temp_endFrame : asm_program =
    [
      VorL "LCL"; 
      string_to_CInstr (ref "D=M");   (* LCL *)
      VorL "R14"; 
      string_to_CInstr (ref "M=D");   (* R14 | endFrame = LCL *)
    ]
  
  let temp_retAddr : asm_program =
    [ 
      VorL "R14"; 
      string_to_CInstr (ref "D=M");    (* endFrame *)
      At 5; 
      string_to_CInstr (ref "A=D-A"); (* endFrame - 5 *)
      string_to_CInstr (ref "D=M");   (* *(endFrame - 5) *)
      VorL "R15"; 
      string_to_CInstr (ref "M=D")    (* R15 | retAddr = *(endFrame -5) *)
  ]

  let pop_to_arg : asm_program =
    [
      VorL "SP";
      string_to_CInstr (ref "AM=M-1");
      string_to_CInstr (ref "D=M"); (* pop() *)
      VorL "ARG";
      string_to_CInstr (ref "A=M");
      string_to_CInstr (ref "M=D"); (* *ARG = pop() *)
    ]
  
  let repositionSP : asm_program =
    [ 
      VorL "ARG"; 
      string_to_CInstr (ref "D=M+1"); (* ARG + 1 *)
      VorL "SP"; 
      string_to_CInstr (ref "M=D"); (* SP = ARG + 1 *)
    ]
  
  let restoreTHAT : asm_program =
    [
      VorL "R14";
      string_to_CInstr (ref "AM=M-1"); (* endFrame = endFrame - 1 *)
      string_to_CInstr (ref "D=M");  
      VorL "THAT";
      string_to_CInstr (ref "M=D");   (* THAT = *(endFrame - 1) *)
    ]

  let restoreTHIS : asm_program =
    [
      VorL "R14";
      string_to_CInstr (ref "AM=M-1"); (* endFrame = endFrame - 2 *)
      string_to_CInstr (ref "D=M");  
      VorL "THIS";
      string_to_CInstr (ref "M=D");   (* THIS = *(endFrame - 2) *)
    ]

  let restoreARG : asm_program =
    [
      VorL "R14";
      string_to_CInstr (ref "AM=M-1"); (* endFrame = endFrame - 3 *)
      string_to_CInstr (ref "D=M");  
      VorL "ARG";
      string_to_CInstr (ref "M=D");   (* ARG = *(endFrame - 3) *)
    ]
  
  let restoreLCL : asm_program =
    [
      VorL "R14";
      string_to_CInstr (ref "AM=M-1"); (* endFrame = endFrame - 4 *)
      string_to_CInstr (ref "D=M");  
      VorL "LCL";
      string_to_CInstr (ref "M=D");   (* LCL = *(endFrame - 4) *)
    ]
  
  let gotoretAddr : asm_program =
    [
      VorL "R15";
      string_to_CInstr (ref "A=M"); 
      string_to_CInstr (ref "0;JMP"); 
    ]

  let return : asm_program =
    List.concat [
      temp_endFrame;       (* Extract endFrame and store it in R14 *)
      temp_retAddr;        (* Calculate the return address (endFrame - 5) and store in R15*)
      pop_to_arg;          (* Pop the value from the stack and store it in ARG *)
      repositionSP;        (* Set SP to ARG + 1 *)
      restoreTHAT;         (* Restore THAT pointer from (endFrame - 1) *)
      restoreTHIS;         (* Restore THIS pointer from (endFrame - 2) *)
      restoreARG;          (* Restore ARG pointer from (endFrame - 3) *)
      restoreLCL;          (* Restore LCL pointer from (endFrame - 4) *)
      gotoretAddr;         (* Jump to the return address stored in R15 *)
    ]
end


module Token_to_Asm = struct
  let length_1 (i:string) : vm_statement =
    match i with
    | "add"     -> Operator Add
    | "sub"     -> Operator Sub
    | "and"     -> Operator And
    | "or"      -> Operator Or
    | "neg"     -> Operator Neg
    | "not"     -> Operator Not
    | "eq"      -> Operator Eq
    | "lt"      -> Operator Lt
    | "return"  -> Return

    | _ -> failwith (__FUNCTION__ ^ ": only works for unary, arithmetic, bitwise & return statements")


  let length_2 (i:string) (lbl:string) : vm_statement =
    match i with
    | "label"     -> Label    lbl
    | "goto"      -> Goto     lbl
    | "if-goto"   -> Ifgoto   lbl

    | _ -> failwith (__FUNCTION__ ^ ": only works for label, goto & ifgoto")

    
  let length_3 (i:string) (j:string) (k:string) : vm_statement =
    match i with
    | "function"         -> Function    (j,                   int_of_string k)
    | "call"             -> Call        (j,                   int_of_string k)
    | "push"             -> Push        (strseg_to_vmseg j,   int_of_string k)
    | "pop"              -> Pop         (strseg_to_vmseg j,   int_of_string k)
    | _ -> failwith (__FUNCTION__ ^ ": only works for function, call, push & pop statements")


  let translator (l: string list) : vm_statement =
    let length = List.length l 
    in
    match length with
    | 1 -> length_1     (List.nth l 0)
    | 2 -> length_2     (List.nth l 0)    (List.nth l 1)
    | 3 -> length_3     (List.nth l 0)    (List.nth l 1)    (List.nth l 2)
    | _ -> failwith (__FUNCTION__ ^ ": List should not be empty or have >=4 terms")
end



let instr_to_asm (fileName: string) (funcName : string) (comp_c : int) (call_c: int) (instr: vm_statement) : asm_program =
  let prefix = fileName ^ "." ^ funcName ^ "$" in
  match instr with
  | Push      (seg,index)       -> Push_Helper.push     fileName  seg   index
  | Pop       (seg,index)       -> Pop_Helper.pop       fileName  seg   index
  
  | Goto         lbl            -> Branch_Helper.goto     (prefix ^ lbl)
  | Ifgoto       lbl            -> Branch_Helper.ifgoto   (prefix ^ lbl)
  | Label        lbl            -> Label_Helper.label     (prefix ^ lbl)

  | Operator     op             -> Operation_Helper.oper   fileName   funcName  comp_c   op
  
  | Call       (name,nArgs)     -> Call_Helper.call           fileName   name   call_c   nArgs
  | Function   (name,nVars)     -> Function_Helper.preamble   fileName   name   nVars
  | Return                      -> Return_Helper.return 
  