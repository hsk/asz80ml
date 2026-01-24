open Ast
open Macro
(* 命令をバイト列に変換する (Pass 2用) *)
let encode env instr =
  match instr.operand with
  | Expr ("nop", []) -> [0x00]
  | Expr ("ret", []) -> [0xC9]
  | Expr ("ld", [Var "a"; src]) ->
      begin match eval_expr env src with
      | Paren (Add (Var "ix", Int d)) -> [0x7E; d land 0xFF]
      | Int n -> [0x3E; n land 0xFF]
      | _ -> []
      end
  | _ -> []

(* 命令のサイズを返す (Pass 1用) *)
let get_size env instr =
  List.length (encode env instr)

let eval_expr env expr =
  match eval_expr env expr with
  | Int n -> n
  | _ -> assert false

(* Pass 1: シンボルテーブルの作成 *)
let pass1 prog =
  let rec loop addr env = function
    | [] -> env
    | instr :: rest ->
        match instr.operand with
        | Label name ->
            loop addr ((name, Int addr) :: env) rest
        | Expr ("org", [addr_expr]) ->
            let new_addr = eval_expr env addr_expr in
            loop new_addr env rest
        | _ ->
            let size = get_size env instr in
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
            let new_addr = eval_expr env addr_expr in
            Printf.printf "                org 0x%04X\n" new_addr;
            loop new_addr rest
        | _ ->
            let bytes = encode env instr in
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

let assemble prog =
  let env = pass1 prog in
  pass2 prog env
