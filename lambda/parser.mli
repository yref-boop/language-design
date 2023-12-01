type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | CONCAT
  | FIRST
  | SUB
  | BOOL
  | NAT
  | TUPLE
  | STRING
  | CHAR
  | FIX
  | LPAREN
  | RPAREN
  | LCURLY
  | RCURLY
  | COMMA
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)
  | CHARV of (char)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
