open Ast
open Macro

let eval_expr_int env expr =
  match eval_expr env expr with
  | Int n -> n
  | _ -> assert false

let expr env pc instr =
  let v_ = ref 0 in
  let abcdehl = function
    | Var "b" -> v_ := 0; true
    | Var "c" -> v_ := 1; true
    | Var "d" -> v_ := 2; true
    | Var "e" -> v_ := 3; true
    | Var "h" -> v_ := 4; true
    | Var "l" -> v_ := 5; true
    | Var "a" -> v_ := 7; true
    | _ -> false
  in
  let v2_ = ref 0 in
  let abcdehlref = function
    | Var "b" -> v2_ := 0; true
    | Var "c" -> v2_ := 1; true
    | Var "d" -> v2_ := 2; true
    | Var "e" -> v2_ := 3; true
    | Var "h" -> v2_ := 4; true
    | Var "l" -> v2_ := 5; true
    | Paren(Var "hl") -> v2_ := 6; true
    | Var "a" -> v2_ := 7; true
    | _ -> false
  in
  let ixiy_ = ref 0 in
  let ixiy = function
    | Var "ix" -> ixiy_ := 0xdd; true
    | Var "iy" -> ixiy_ := 0xfd; true
    | _ -> false
  in
  let bc_de = function
    | Var "bc" -> v_ := 0x00; true
    | Var "de" -> v_ := 0x10; true
    | _ -> false
  in
  let bc_de_sp = function
    | Var "bc" -> v_ := 0x00; true
    | Var "de" -> v_ := 0x10; true
    | Var "sp" -> v_ := 0x30; true
    | _ -> false
  in
  let bc_de_hl_sp = function
    | Var "bc" -> v_ := 0x00; true
    | Var "de" -> v_ := 0x10; true
    | Var "hl" -> v_ := 0x20; true
    | Var "sp" -> v_ := 0x30; true
    | _ -> false
  in
  let bc_de_hl_af = function
    | Var "bc" -> v_ := 0x00; true
    | Var "de" -> v_ := 0x10; true
    | Var "hl" -> v_ := 0x20; true
    | Var "af" -> v_ := 0x30; true
    | _ -> false
  in
  let bc_de_ix_sp r = function
    | Var "bc" -> v_ := 0x00; true
    | Var "de" -> v_ := 0x10; true
    | r1 when r1=r -> v_ := 0x20; true
    | Var "sp" -> v_ := 0x30; true
    | _ -> false
  in
  let flg = function
    | Var "nz" -> v_ := 0x00; true
    | Var "z"  -> v_ := 0x08; true
    | Var "nc" -> v_ := 0x10; true
    | Var "c"  -> v_ := 0x18; true
    | Var "po" -> v_ := 0x20; true
    | Var "pe" -> v_ := 0x28; true
    | Var "p"  -> v_ := 0x30; true
    | Var "m"  -> v_ := 0x38; true
    | _ -> false
  in
  let int_list_of_string s =
    List.init (String.length s) (fun i ->
      Char.code s.[i]
    )
  in
  let int = function
    | Int a -> a
    | String x ->
      (match int_list_of_string x with
      | [i] -> i
      | _ -> assert false)
    | _ -> assert false
  in
  let u8 i = (int i) land 255
  in
  let u16 i =
    let i = int i in
    [i mod 0x100;i / 256]
  in
  let short_jmp = function
    | Int a ->
      let v = (a - pc - 2) in
      if v < -128 || 127 < v then failwith ((show_instruction instr)^" jump address range -128 127");
      v land 255
    | _ -> assert false
  in
  let n, args = match instr.operand with
  | Expr (n,a) -> (n,a)
  | _ -> failwith (show_instruction instr)
  in
  match n, List.map (eval_expr env) args with
  | ("adc",[Var "a";r]) when abcdehlref r -> [0x88 + !v2_]
  | ("adc",[Var "a";Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0x8e;u8 d]
  | ("adc",[Var "a";i]) -> [0xce;u8 i]
  | ("adc",[Var "hl";r]) when bc_de_hl_sp r -> [0xed;0x4a + !v_]
  | ("add",[Var "a";r]) when abcdehlref r -> [0x80 + !v2_]
  | ("add",[Var "a";Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0x86;u8 d]
  | ("add",[Var "a";i]) -> [0xc6;u8 i]
  | ("add",[Var "hl";r]) when bc_de_hl_sp r -> [0x09 + !v_]
  | ("add",[r;r2]) when ixiy r && bc_de_ix_sp r r2 -> [!ixiy_;0x09 + !v_]
  | ("and",[r]) when abcdehlref r -> [0xa0 + !v2_]
  | ("and",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xa6;u8 d]
  | ("and",[i]) -> [0xe6;u8 i]
  | ("bit",[i;r]) when abcdehlref r ->
    let i = int i in
    if 0 <= i && i <= 7 then [0xcb;0x40 + i * 8+ !v2_] else failwith ((show_instruction instr)^" range error")
  | ("bit",[i;Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x46 + (int i) * 8]
  | ("call",[f;i]) when flg f -> (0xc4 + !v_ :: u16 i)
  | ("call",[i]) -> (0xcd :: u16 i)
  | ("ccf",[]) -> [0x3f]
  | ("cp",[r]) when abcdehlref r -> [0xb8 + !v2_]
  | ("cp",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xbe;u8 d]
  | ("cp",[i]) -> [0xfe;u8 i]
  | ("cpd",[]) -> [0xed;0xa9]
  | ("cpdr",[]) -> [0xed;0xb9]
  | ("cpi",[]) -> [0xed;0xa1]
  | ("cpir",[]) -> [0xed;0xb1]
  | ("cpl",[]) -> [0x2f]
  | ("daa",[]) -> [0x27]
  | ("dec",[r]) when abcdehl r -> [0x05 + !v_ * 8]
  | ("dec",[Paren(Var "hl")]) -> [0x35]
  | ("dec",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0x35;u8 d]
  | ("dec",[r]) when bc_de_hl_sp r -> [0x0b + !v_]
  | ("dec",[r]) when ixiy r -> [!ixiy_;0x2b]
  | ("di",[]) -> [0xf3]
  | ("djnz",[e]) -> [0x10;short_jmp e] 
  | ("ei",[]) -> [0xfb]
  | ("ex",[Paren(Var "sp");Var "hl"]) -> [0xe3]
  | ("ex",[Paren(Var "sp");r]) when ixiy r -> [!ixiy_;0xe3]
  | ("ex",[Var "af";Var "af'"]) -> [0x08]
  | ("ex",[Var "de";Var "hl"]) -> [0xeb]
  | ("exx",[]) -> [0xd9]
  | ("halt",[]) -> [0x76]
  | ("im", [Int 0]) -> [0xED; 0x46]
  | ("im", [Int 1]) -> [0xED; 0x56]
  | ("im", [Int 2]) -> [0xED; 0x5E]
  | ("inc",[r]) when abcdehl r -> [0x04 + !v_ * 8]
  | ("inc",[Paren(Var "hl")]) -> [0x34]
  | ("inc",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0x34;u8 d]
  | ("inc",[r]) when bc_de_hl_sp r -> [0x03 + !v_]
  | ("inc",[r]) when ixiy r -> [!ixiy_;0x23]
  | ("in",[r;Paren(Var "c")]) when abcdehl r -> [0xed;0x40 + !v_ * 8]
  | ("in",[Var "a";Paren(i)]) -> [0xdb;u8 i]
  | ("ind",[]) -> [0xed;0xaa]
  | ("indr",[]) -> [0xed;0xba]
  | ("ini",[]) -> [0xed;0xa2]
  | ("inir",[]) -> [0xed;0xb2]
  | ("jp",[Paren(Var "hl")]) -> [0xe9]
  | ("jp",[Paren(r)]) when ixiy r -> [!ixiy_;0xe9]
  | ("jp",[i]) -> (0xc3::u16 i)
  | ("jp",[f;i]) when flg f -> (0xc2 + !v_ :: u16 i)
  | ("jr",[e]) -> [0x18;short_jmp e]
  | ("jr",[Var "nz";e]) -> [0x20;short_jmp e]
  | ("jr",[Var "z";e]) -> [0x28;short_jmp e]
  | ("jr",[Var "nc";e]) -> [0x30;short_jmp e]
  | ("jr",[Var "c";e]) -> [0x38;short_jmp e]
  | ("ld", [r;r2]) when abcdehl r && abcdehlref r2 -> [0x40 + !v_ * 8 + !v2_]
  | ("ld",[r;Paren(Add(r2,d))]) when abcdehl r && ixiy r2 -> [!ixiy_;0x46 + !v_ * 8;u8 d]
  | ("ld",[Var "a";Paren(r)]) when bc_de r -> [0x0a + !v_]
  | ("ld",[Var "a";Paren(i)]) -> (0x3a::u16 i)
  | ("ld",[Var "a";Var "i"]) -> [0xed;0x57]
  | ("ld",[Var "a";Var "r"]) -> [0xed;0x5f]
  | ("ld",[Var "i";Var "a"]) -> [0xed;0x47]
  | ("ld",[Var "r";Var "a"]) -> [0xed;0x4f]
  | ("ld",[r;i]) when abcdehl r -> [0x06 + !v_ * 8;u8 i]
  | ("ld",[Paren(r);Var "a"]) when bc_de r -> [0x02 + !v_]
  | ("ld",[Paren(Var "hl");r]) when abcdehl r -> [0x70 + !v_]
  | ("ld",[Paren(Var "hl");i]) -> [0x36;u8 i]
  | ("ld",[Paren(Add(r,d));r2]) when ixiy r && abcdehl r2 -> [!ixiy_;0x70 + !v_;u8 d]
  | ("ld",[Paren(Add(r,d));i]) when ixiy r -> [!ixiy_;0x36;u8 d;u8 i]
  | ("ld",[Paren(i);Var "a"]) -> (0x32::u16 i)
  | ("ld",[Var "sp";Var "hl"]) -> [0xf9]
  | ("ld",[Var "sp";r]) when ixiy r -> [!ixiy_;0xf9]
  | ("ld",[Var "hl";Paren(i)]) -> ([0x2a] @ u16 i)
  | ("ld",[r;Paren(i)]) when bc_de_sp r -> ([0xed;0x4b + !v_] @ u16 i)
  | ("ld",[r;Paren(i)]) when ixiy r -> ([!ixiy_;0x2a] @ u16 i)
  | ("ld",[r;i]) when bc_de_hl_sp r -> ([0x01 + !v_] @ u16 i)
  | ("ld",[r;i]) when ixiy r -> ([!ixiy_;0x21] @ u16 i)
  | ("ld",[Paren(i);Var "hl"]) -> ([0x22] @ u16 i)
  | ("ld",[Paren(i);r]) when bc_de_sp r -> ([0xed;0x43 + !v_] @ u16 i)
  | ("ld",[Paren(i);r]) when ixiy r -> ([!ixiy_;0x22] @ u16 i)
  | ("ldd",[]) -> [0xed;0xa8]
  | ("lddr",[]) -> [0xed;0xb8]
  | ("ldi",[]) -> [0xed;0xa0]
  | ("ldir",[]) -> [0xed;0xb0]
  | ("neg",[]) -> [0xed;0x44]
  | ("nop",[]) -> [0x00]
  | ("or",[r]) when abcdehlref r -> [0xb0 + !v2_]
  | ("or",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xb6;u8 d]
  | ("or",[i]) -> [0xf6;u8 i]
  | ("out",[Paren(Var "c");r]) when abcdehl r -> [0xed;0x41 + !v_ * 8]
  | ("out",[Paren(i);Var "a"]) -> [0xd3;u8 i]
  | ("outd",[]) -> [0xed;0xab]
  | ("otdr",[]) -> [0xed;0xbb]
  | ("outi",[]) -> [0xed;0xa3]
  | ("otir",[]) -> [0xed;0xb3]
  | ("pop",[r]) when bc_de_hl_af r -> [0xc1 + !v_]
  | ("pop",[r]) when ixiy r -> [!ixiy_;0xe1]
  | ("push",[r]) when bc_de_hl_af r -> [0xc5 + !v_]
  | ("push",[r]) when ixiy r -> [!ixiy_;0xe5]
  | ("res",[i;r]) when abcdehlref r ->
    let i = int i in
    if 0 <= i && i <= 7 then [0xcb;0x80 + i * 8+ !v2_] else failwith ((show_instruction instr)^" 0 <= i <= 7");
  | ("res",[i;Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x86 + (int i) * 8]
  | ("ret",[]) -> [0xc9]
  | ("ret",[f]) when flg f -> [0xc0 + !v_]
  | ("reti",[]) -> [0xed;0x4d]
  | ("retn",[]) -> [0xed;0x45]
  | ("rla",[]) -> [0x17]
  | ("rlca",[]) -> [0x07]
  | ("rra",[]) -> [0x1f]
  | ("rrca",[]) -> [0x0f]
  | ("rl",[r]) when abcdehlref r -> [0xcb;0x10 + !v2_]
  | ("rl",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x16]
  | ("rlc",[r]) when abcdehlref r -> [0xcb;0x00 + !v2_]
  | ("rlc",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x06]
  | ("rr",[r]) when abcdehlref r -> [0xcb;0x18 + !v2_]
  | ("rr",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x1e]
  | ("rrc",[r]) when abcdehlref r -> [0xcb;0x08 + !v2_]
  | ("rrc",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x0e]
  | ("rld",[]) -> [0xed;0x6f]
  | ("rrd",[]) -> [0xed;0x67]
  | ("rst",[i]) ->
    let i = int i in
    if i mod 8 = 0 && 0 <= i && i <= 0x38 then [0xc7 + i] else failwith ((show_instruction instr)^" range")
  | ("sbc",[Var "a";r]) when abcdehlref r -> [0x98 + !v2_]
  | ("sbc",[Var "a";Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0x9e;u8 d]
  | ("sbc",[Var "a";i]) -> [0xde;u8 i]
  | ("sbc",[Var "hl";r]) when bc_de_hl_sp r -> [0xed;0x42 + !v_]
  | ("scf",[]) -> [0x37]
  | ("set",[i;r]) when abcdehlref r ->
    let i = int i in
    if 0 <= i && i <= 7 then [0xcb;0xc0 + i * 8+ !v2_] else  failwith ((show_instruction instr)^" 0 <= i <= 7");
  | ("set",[i;Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0xc6 + (int i) * 8]
  | ("sla",[r]) when abcdehlref r -> [0xcb;0x20 + !v2_]
  | ("sla",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x26]
  | ("sra",[r]) when abcdehlref r -> [0xcb;0x28 + !v2_]
  | ("sra",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x2e]
  | ("srl",[r]) when abcdehlref r -> [0xcb;0x38 + !v2_]
  | ("srl",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xcb;u8 d;0x3e]
  | ("sub",[r]) when abcdehlref r -> [0x90 + !v2_]
  | ("sub",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0x96;u8 d]
  | ("sub",[i]) -> [0xd6;u8 i]
  | ("xor",[r]) when abcdehlref r -> [0xa8 + !v2_]
  | ("xor",[Paren(Add(r,d))]) when ixiy r -> [!ixiy_;0xae;u8 d]
  | ("xor",[i]) -> [0xee;u8 i]
  | _ -> []

(* 命令をバイト列に変換する (Pass 2用) *)
let encode env pc instr =
  match instr.operand with
  | Expr ("defb", args) | Expr ("db", args) | Expr ("defm", args) | Expr ("dm", args) ->
      List.map (fun e ->
        match eval_expr env e with
        | Int n -> [n land 0xFF]
        | String s -> List.init (String.length s) (fun i -> Char.code s.[i])
        | _ -> [0]
      ) args |> List.flatten
  | Expr ("defw", args) | Expr ("dw", args) ->
      List.concat (List.map (fun e -> 
        match eval_expr env e with
        | Int n -> [n land 0xFF; (n lsr 8) land 0xFF]
        | _ -> [0; 0]
      ) args)
  | Expr ("defs", [src]) | Expr ("ds", [src]) ->
      List.init (eval_expr_int env src) (fun _ -> 0)
  | Expr ("defs", [src;v]) | Expr ("ds", [src;v]) ->
      List.init (eval_expr_int env src) (fun _ -> (eval_expr_int env v))
  | Expr ("incbin", [src]) ->
      let filename = match eval_expr env src with
        | String s -> s
        | _ -> failwith "incbin: filename must be a string"
      in
      let ic = open_in_bin filename in
      let len = in_channel_length ic in
      let buf = Bytes.create len in
      really_input ic buf 0 len;
      close_in ic;
      List.init len (fun i -> int_of_char (Bytes.get buf i))
  | _ -> expr env pc instr
(* 命令のサイズを返す (Pass 1用) *)
let get_size env pc instr =
  List.length (encode env pc instr)

(* Pass 1: シンボルテーブルの作成 *)
let pass1 prog =
  let rec loop addr env = function
    | [] -> env
    | instr :: rest ->
        match instr.operand with
        | Label name ->
            loop addr ((name, Int addr) :: env) rest
        | Expr ("org", [addr_expr]) ->
            let new_addr = eval_expr_int env addr_expr in
            loop new_addr env rest
        | _ ->
            let size = get_size env addr instr in
            loop (addr + size) env rest
  in
  loop 0 [] prog

(* Pass 2: コード生成と表示 *)
let pass2 prog env =
  let rec loop addr = function
    | [] -> ()
    | instr :: rest ->
        match instr.operand with
        | Label name ->
            Printf.printf "%04X          %s:\n" addr name;
            loop addr rest
        | Expr ("org", [addr_expr]) ->
            let new_addr = eval_expr_int env addr_expr in
            Printf.printf "                org 0x%04X\n" new_addr;
            loop new_addr rest
        | _ ->
            let bytes = encode env addr instr in
            let bytes_str = 
              if bytes = [] then "" 
              else String.concat " " (List.map (Printf.sprintf "%02X") bytes)
            in
            (* アセンブリソースの表示用 *)
            let src_str = Ast.show_operand instr.operand in
            
            if bytes <> [] then
              Printf.printf "%04X  %-11s  %s\n" addr bytes_str src_str;
            
            let size = List.length bytes in
            loop (addr + size) rest
  in
  loop 0 prog

(* Pass 3: Binary Output *)
let pass3 prog env filename =
  let oc = open_out_bin filename in
  let rec loop addr = function
    | [] -> ()
    | instr :: rest ->
        match instr.operand with
        | Label _ -> loop addr rest
        | Expr ("org", [addr_expr]) ->
            let new_addr = eval_expr_int env addr_expr in
            loop new_addr rest
        | _ ->
            let bytes = encode env addr instr in
            List.iter (output_byte oc) bytes;
            loop (addr + List.length bytes) rest
  in
  try
    loop 0 prog;
    close_out oc;
    Printf.printf "Binary output written to %s\n" filename
  with e ->
    close_out_noerr oc;
    Printf.eprintf "Error writing binary file: %s\n" (Printexc.to_string e);
    raise e

let assemble prog out_file =
  let env = pass1 prog in
  pass2 prog env;
  match out_file with
  | Some f -> pass3 prog env f
  | None -> ()
