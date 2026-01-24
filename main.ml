(* Parse a file and return the program *)
let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  
  (* Reset and set line number and filename *)
  Parser.line := 0;
  Parser.file := filename;
  
  try
    let program = Parser.main Lexer.token lexbuf in
    close_in ic;
    program
  with
  | Parser.Error ->
      close_in ic;
      Printf.eprintf "Parse error at line %d\n" !Parser.line;
      raise (Failure "Parse error")
  | e ->
      close_in ic;
      raise e

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1
  );
  
  let filename = Sys.argv.(1) in
  
  try
    let program = parse_file filename in
    
    Printf.printf "Parse successful!\n";
    Printf.printf "Original program:\n%s\n\n" (Ast.show_program program);
    
    (* Evaluate with empty environment *)
    let evaluated = Macro.eval_program [] program in
    Printf.printf "Evaluated program:\n%s\n" (Ast.show_program evaluated)
  with
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Failure msg ->
      exit 1
