
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "fix"       { FIX }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "first"     { FIRST }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "Tuple"     { TUPLE }
  | "Record"    { RECORD }
  | "Char"      { CHAR }
  | "sub"       { SUB }
  | "List"      {LIST}
  | "null"      {NULL}
  | "l."        {LISTV}
  | "isEmpty"   {ISEMPTY}
  | "head"      {HEAD}
  | "tail"      {TAIL}
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LCURLY }
  | '}'         { RCURLY }
  | '['         { LSQUARE }
  | ']'         { RSQUARE }
  | ','         { COMMA }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | '"'[^'"' ';' '\n']*'"'
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s - 2)) }
  | ''' [^';' ''' '"' '\n'] '''   
                { let c = Lexing.lexeme lexbuf in
                        CHARV (c.[1])}
  | eof         { EOF }
  | _           { raise Lexical_error }
