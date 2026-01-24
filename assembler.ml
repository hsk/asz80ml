open Ast
open Macro

let eval_expr_int env expr =
  match eval_expr env expr with
  | Int n -> n
  | _ -> assert false

let z80_encode env pc instr =
  let val_of e = eval_expr_int env e in
  let byte n = n land 0xFF in
  let word n = [n land 0xFF; (n lsr 8) land 0xFF] in
  let rel_disp dest = 
    let offset = dest - (pc + 2) in
    if offset < -128 || offset > 127 then
      Printf.eprintf "Warning: Relative jump out of range at %04X\n" pc;
    offset land 0xFF
  in

  (* Helpers for operand decoding *)
  let r8 expr = 
    match expr with
    | Var "b" -> Some 0 | Var "c" -> Some 1 | Var "d" -> Some 2 | Var "e" -> Some 3
    | Var "h" -> Some 4 | Var "l" -> Some 5 | Paren (Var "hl") -> Some 6 | Var "a" -> Some 7
    | _ -> None 
  in
  let ixiy expr =
    match expr with
    | Paren (Add (Var "ix", d)) | Paren (Add (d, Var "ix")) -> Some (0xDD, val_of d)
    | Paren (Add (Var "iy", d)) | Paren (Add (d, Var "iy")) -> Some (0xFD, val_of d)
    | Paren (Var "ix") -> Some (0xDD, 0)
    | Paren (Var "iy") -> Some (0xFD, 0)
    | _ -> None
  in
  let r8_ext expr = (* Returns (prefix, opcode_bits, disp) *)
    match r8 expr with
    | Some c -> (None, c, None)
    | None -> 
        match ixiy expr with
        | Some (pre, d) -> (Some pre, 6, Some d)
        | None -> raise Not_found
  in
  let r16_sp expr = match expr with Var "bc" -> Some 0 | Var "de" -> Some 1 | Var "hl" -> Some 2 | Var "sp" -> Some 3 | _ -> None in
  let r16_af expr = match expr with Var "bc" -> Some 0 | Var "de" -> Some 1 | Var "hl" -> Some 2 | Var "af" -> Some 3 | _ -> None in
  let cc expr = match expr with
    | Var "nz" -> Some 0 | Var "z" -> Some 1 | Var "nc" -> Some 2 | Var "c" -> Some 3
    | Var "po" -> Some 4 | Var "pe" -> Some 5 | Var "p" -> Some 6 | Var "m" -> Some 7
    | _ -> None
  in
  
  (* Common ALU encoding *)
  let alu_op base_op imm_op args =
    match args with
    | [Var "a"; src] | [src] ->
        begin try
          let (pre, code, disp) = r8_ext src in
          let op = base_op + code in
          match pre, disp with
          | Some p, Some d -> [p; op; byte d]
          | _, _ -> [op]
        with Not_found ->
          [imm_op; byte (val_of src)]
        end
    | _ -> []
  in

  match instr.operand with
  | Expr ("nop", []) -> [0x00]
  | Expr ("halt", []) -> [0x76]
  | Expr ("di", []) -> [0xF3]
  | Expr ("ei", []) -> [0xFB]
  | Expr ("ret", []) -> [0xC9]
  | Expr ("reti", []) -> [0xED; 0x4D]
  | Expr ("retn", []) -> [0xED; 0x45]
  | Expr ("exx", []) -> [0xD9]
  | Expr ("scf", []) -> [0x37]
  | Expr ("ccf", []) -> [0x3F]
  | Expr ("rla", []) -> [0x17]
  | Expr ("rlca", []) -> [0x07]
  | Expr ("rra", []) -> [0x1F]
  | Expr ("rrca", []) -> [0x0F]
  | Expr ("cpl", []) -> [0x2F]
  | Expr ("neg", []) -> [0xED; 0x44]
  | Expr ("daa", []) -> [0x27]
  
  (* 8-bit Load *)
  | Expr ("ld", [dst; src]) ->
      begin try
        (* ld r, r' / ld r, (hl) / ld r, (ix+d) *)
        match r8_ext dst, r8_ext src with
        | (pre1, r1, disp1), (pre2, r2, disp2) ->
            if pre1 <> None && pre2 <> None then [] (* Invalid ld (ix+d), (ix+d) *)
            else if pre1 <> None then
              (* ld (ix+d), r *)
              match disp1 with Some d -> [Option.get pre1; 0x70 + r2; byte d] | _ -> []
            else if pre2 <> None then
              (* ld r, (ix+d) *)
              match disp2 with Some d -> [Option.get pre2; 0x40 + r1 * 8 + 6; byte d] | _ -> []
            else
              (* ld r, r' *)
              [0x40 + r1 * 8 + r2]
      with Not_found ->
        (* ld r, n / ld (hl), n / ld (ix+d), n *)
        try
          let (pre, r, disp) = r8_ext dst in
          let n = val_of src in
          match pre, disp with
          | Some p, Some d -> [p; 0x36; byte d; byte n]
          | _, _ -> [0x06 + r * 8; byte n]
        with Not_found ->
          (* ld a, (nn) / ld (nn), a / ld hl, (nn) ... *)
          match dst, src with
          | Var "a", Paren (nn) -> [0x3A] @ word (val_of nn)
          | Paren (nn), Var "a" -> [0x32] @ word (val_of nn)
          | Var "bc", Paren (nn) -> [0xED; 0x4B] @ word (val_of nn)
          | Var "de", Paren (nn) -> [0xED; 0x5B] @ word (val_of nn)
          | Var "hl", Paren (nn) -> [0x2A] @ word (val_of nn)
          | Var "sp", Paren (nn) -> [0xED; 0x7B] @ word (val_of nn)
          | Paren (nn), Var "bc" -> [0xED; 0x43] @ word (val_of nn)
          | Paren (nn), Var "de" -> [0xED; 0x53] @ word (val_of nn)
          | Paren (nn), Var "hl" -> [0x22] @ word (val_of nn)
          | Paren (nn), Var "sp" -> [0xED; 0x73] @ word (val_of nn)
          (* ld r16, nn *)
          | _, _ ->
              match r16_sp dst with
              | Some r -> [0x01 + r * 16] @ word (val_of src)
              | None -> 
                  match ixiy dst with
                  | Some (pre, _) -> [pre; 0x21] @ word (val_of src)
                  | None -> []
      end

  (* 16-bit Load/Stack *)
  | Expr ("push", [src]) ->
      begin match r16_af src with Some r -> [0xC5 + r * 16] | None -> 
        match ixiy src with Some (pre, _) -> [pre; 0xE5] | None -> [] end
  | Expr ("pop", [dst]) ->
      begin match r16_af dst with Some r -> [0xC1 + r * 16] | None -> 
        match ixiy dst with Some (pre, _) -> [pre; 0xE1] | None -> [] end
  | Expr ("ex", [Var "de"; Var "hl"]) -> [0xEB]
  | Expr ("ex", [Var "af"; Var "af'"]) -> [0x08]
  | Expr ("ex", [Paren (Var "sp"); Var "hl"]) -> [0xE3]
  | Expr ("ex", [Paren (Var "sp"); src]) ->
      begin match ixiy src with Some (pre, _) -> [pre; 0xE3] | None -> [] end

  (* ALU *)
  | Expr ("add", [Var "hl"; src]) ->
      begin match r16_sp src with Some r -> [0x09 + r * 16] | None -> [] end
  | Expr ("add", [dst; src]) when (match ixiy dst with Some _ -> true | _ -> false) ->
      begin match ixiy dst, r16_sp src with
      | Some (pre, _), Some r -> [pre; 0x09 + r * 16]
      | _ -> alu_op 0x80 0xC6 [dst; src]
      end
  | Expr ("adc", [Var "hl"; src]) ->
      begin match r16_sp src with Some r -> [0xED; 0x4A + r * 16] | None -> [] end
  | Expr ("sbc", [Var "hl"; src]) ->
      begin match r16_sp src with Some r -> [0xED; 0x42 + r * 16] | None -> [] end
  
  | Expr ("add", args) -> alu_op 0x80 0xC6 args
  | Expr ("adc", args) -> alu_op 0x88 0xCE args
  | Expr ("sub", args) -> alu_op 0x90 0xD6 args
  | Expr ("sbc", args) -> alu_op 0x98 0xDE args
  | Expr ("and", args) -> alu_op 0xA0 0xE6 args
  | Expr ("xor", args) -> alu_op 0xA8 0xEE args
  | Expr ("or", args) -> alu_op 0xB0 0xF6 args
  | Expr ("cp", args) -> alu_op 0xB8 0xFE args

  (* Inc/Dec *)
  | Expr ("inc", [dst]) ->
      begin try
        let (pre, r, disp) = r8_ext dst in
        match pre, disp with
        | Some p, Some d -> [p; 0x34; byte d]
        | _, _ -> [0x04 + r * 8]
      with Not_found ->
        match r16_sp dst with Some r -> [0x03 + r * 16] | None ->
        match ixiy dst with Some (pre, _) -> [pre; 0x23] | None -> []
      end
  | Expr ("dec", [dst]) ->
      begin try
        let (pre, r, disp) = r8_ext dst in
        match pre, disp with
        | Some p, Some d -> [p; 0x35; byte d]
        | _, _ -> [0x05 + r * 8]
      with Not_found ->
        match r16_sp dst with Some r -> [0x0B + r * 16] | None ->
        match ixiy dst with Some (pre, _) -> [pre; 0x2B] | None -> []
      end

  (* Control Flow *)
  | Expr ("jp", [dst]) ->
      begin match dst with
      | Paren (Var "hl") -> [0xE9]
      | _ -> 
          match ixiy dst with
          | Some (pre, _) -> [pre; 0xE9]
          | None -> [0xC3] @ word (val_of dst)
      end
  | Expr ("jp", [cond; dst]) ->
      begin match cc cond with
      | Some c -> [0xC2 + c * 8] @ word (val_of dst)
      | None -> []
      end
  | Expr ("jr", [dst]) -> [0x18; rel_disp (val_of dst)]
  | Expr ("jr", [cond; dst]) ->
      begin match cc cond with
      | Some c -> [0x20 + c * 8; rel_disp (val_of dst)]
      | None -> []
      end
  | Expr ("djnz", [dst]) -> [0x10; rel_disp (val_of dst)]
  | Expr ("call", [dst]) -> [0xCD] @ word (val_of dst)
  | Expr ("call", [cond; dst]) ->
      begin match cc cond with
      | Some c -> [0xC4 + c * 8] @ word (val_of dst)
      | None -> []
      end
  | Expr ("rst", [dst]) ->
      let n = val_of dst in
      [0xC7 + (n land 0x38)]

  (* Bit manipulation *)
  | Expr ("bit", [bit; src]) ->
      let b = val_of bit in
      begin try
        let (pre, r, disp) = r8_ext src in
        match pre, disp with
        | Some p, Some d -> [p; 0xCB; byte d; 0x40 + b * 8 + 6]
        | _, _ -> [0xCB; 0x40 + b * 8 + r]
      with Not_found -> []
      end
  | Expr ("set", [bit; src]) ->
      let b = val_of bit in
      begin try
        let (pre, r, disp) = r8_ext src in
        match pre, disp with
        | Some p, Some d -> [p; 0xCB; byte d; 0xC0 + b * 8 + 6]
        | _, _ -> [0xCB; 0xC0 + b * 8 + r]
      with Not_found -> []
      end
  | Expr ("res", [bit; src]) ->
      let b = val_of bit in
      begin try
        let (pre, r, disp) = r8_ext src in
        match pre, disp with
        | Some p, Some d -> [p; 0xCB; byte d; 0x80 + b * 8 + 6]
        | _, _ -> [0xCB; 0x80 + b * 8 + r]
      with Not_found -> []
      end

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
  | _ -> z80_encode env pc instr

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
              Printf.printf "%04X  %-8s  %s\n" addr bytes_str src_str;
            
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
