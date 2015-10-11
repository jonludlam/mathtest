(* Lexer *)
{
  open Parser
}

let digit= ['0'-'9']

rule eqn = parse
  | digit+ 
  | digit+ '.' digit* 
  | '-' digit+
  | '-' digit+ '.'digit*
  | '-' '.' digit+ 
  | '.' digit+ as num { NUM (float_of_string num) }
  | ['a'-'z'] as c { VAR (Printf.sprintf "%c" c) }
  | '\'' ['a'-'z' 'A'-'Z']+ '\'' as s { VAR (String.sub s 1 (String.length s - 2)) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULTIPLY }
  | '/' { DIVIDE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '=' { EQUALS }
  | [' ' '\n' '\t'] { eqn lexbuf }
  | '\\' ['a'-'z' 'A'-'Z' ]+ as id { FUNC id }
  | '#' ['a'-'z' 'A'-'Z' '0'-'9']+ as id { LABEL id }
  | eof { EOF }

{

}
