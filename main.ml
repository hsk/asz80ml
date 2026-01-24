(* Parse a file and return the program *)
let parse_file filename =
  let saved_loc = !Parser.loc in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  
  (* Reset and set line number and filename *)
  Parser.loc := {file = filename; line = 0};

  try
    let program = Parser.main Lexer.token lexbuf in
    close_in ic;
    Parser.loc := saved_loc;
    program
  with
  | Parser.Error ->
      close_in ic;
      Printf.eprintf "Parse error at line %d\n" !Parser.loc.line;
      Parser.loc := saved_loc;
      raise (Failure "Parse error")
  | e ->
      close_in ic;
      Parser.loc := saved_loc;
      raise e

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <filename> [output_binary]\n" Sys.argv.(0);
    exit 1
  );
  
  let filename = Sys.argv.(1) in
  let out_filename = if Array.length Sys.argv > 2 then Some Sys.argv.(2) else None in
  
  try
    let program = parse_file filename in
    
    (* Printf.printf "Parse successful!\n";
    Printf.printf "Original program:\n%s\n\n" (Ast.show_program program);*)
    
    let evaluated = Macro.eval_program parse_file [] program in
    (*Printf.printf "Evaluated program:\n%s\n\n" (Ast.show_program evaluated);

    Printf.printf "Assembly Output:\n";*)
    Assembler.assemble evaluated out_filename
  with
  | Sys_error msg | Failure msg ->
      flush_all ();
      Printf.eprintf "Error: %s\n" msg;
      exit 1
