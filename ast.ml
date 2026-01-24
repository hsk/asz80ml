type expr =
  | Int of int
  | Var of string
  | String of string
  | Ternary of expr * expr * expr  (* a ? b : c *)
  | Or of expr * expr  (* a | b *)
  | Xor of expr * expr  (* a ^ b *)
  | And of expr * expr  (* a & b *)
  | Eq of expr * expr  (* a == b or a = b *)
  | Ne of expr * expr  (* a != b *)
  | Le of expr * expr  (* a <= b *)
  | Ge of expr * expr  (* a >= b *)
  | Lt of expr * expr  (* a < b *)
  | Gt of expr * expr  (* a > b *)
  | LShift of expr * expr  (* a << b *)
  | RShift of expr * expr  (* a >> b *)
  | Add of expr * expr  (* a + b *)
  | Sub of expr * expr  (* a - b *)
  | Mul of expr * expr  (* a * b *)
  | Div of expr * expr  (* a / b *)
  | Mod of expr * expr  (* a % b *)
  | Not of expr  (* ~a *)
  | UAdd of expr  (* +a *)
  | USub of expr  (* -a *)
  | Paren of expr  (* (a) *)

type location = {
  file: string;
  line: int;
}
type operand =
  | Expr of string * expr list
  | Label of string
  | MacroDef of string list * instruction list
  | If of expr * instruction list * instruction list

and instruction = {
  location: location;
  operand: operand;
}

type program = instruction list

let rec show_expr = function
  | Int n -> string_of_int n
  | Var s -> s
  | String s -> Printf.sprintf "%S" s
  | Ternary (a, b, c) -> 
      Printf.sprintf "%s ? %s : %s" (show_expr a) (show_expr b) (show_expr c)
  | Or (a, b) -> Printf.sprintf "%s | %s" (show_expr a) (show_expr b)
  | Xor (a, b) -> Printf.sprintf "%s ^ %s" (show_expr a) (show_expr b)
  | And (a, b) -> Printf.sprintf "%s & %s" (show_expr a) (show_expr b)
  | Eq (a, b) -> Printf.sprintf "%s == %s" (show_expr a) (show_expr b)
  | Ne (a, b) -> Printf.sprintf "%s != %s" (show_expr a) (show_expr b)
  | Le (a, b) -> Printf.sprintf "%s <= %s" (show_expr a) (show_expr b)
  | Ge (a, b) -> Printf.sprintf "%s >= %s" (show_expr a) (show_expr b)
  | Lt (a, b) -> Printf.sprintf "%s < %s" (show_expr a) (show_expr b)
  | Gt (a, b) -> Printf.sprintf "%s > %s" (show_expr a) (show_expr b)
  | LShift (a, b) -> Printf.sprintf "%s << %s" (show_expr a) (show_expr b)
  | RShift (a, b) -> Printf.sprintf "%s >> %s" (show_expr a) (show_expr b)
  | Add (a, Int b) when b < 0 -> Printf.sprintf "%s - %d" (show_expr a) (-b)
  | Add (a, b) -> Printf.sprintf "%s + %s" (show_expr a) (show_expr b)
  | Sub (a, b) -> Printf.sprintf "%s - %s" (show_expr a) (show_expr b)
  | Mul (a, b) -> Printf.sprintf "%s * %s" (show_expr a) (show_expr b)
  | Div (a, b) -> Printf.sprintf "%s / %s" (show_expr a) (show_expr b)
  | Mod (a, b) -> Printf.sprintf "%s %% %s" (show_expr a) (show_expr b)
  | Not a -> Printf.sprintf "~%s" (show_expr a)
  | UAdd a -> Printf.sprintf "+%s" (show_expr a)
  | USub a -> Printf.sprintf "-%s" (show_expr a)
  | Paren a -> Printf.sprintf "(%s)" (show_expr a)

let show_operand = function
  | Expr (mnem, args) ->
      let arg_strs = List.map show_expr args in
      let args_str = String.concat "," arg_strs in
      if args_str = "" then mnem else
      Printf.sprintf "%s %s" mnem args_str
  | Label l -> l ^ ":"
  | MacroDef (params, _body) ->
      Printf.sprintf "macro %s" (String.concat ", " params)
  | If (cond, _, _) -> Printf.sprintf "if %s" (show_expr cond)

let show_instruction instr =
  let op_str = show_operand instr.operand in
  Printf.sprintf "[%s:%d] %s" instr.location.file instr.location.line op_str
(*
let show_program prog =
  String.concat "\n" (List.map show_instruction prog)
*)
