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
  | RECORD
  | LIST
  | STRING
  | CHAR
  | FIX
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | LCURLY
  | RCURLY
  | COMMA
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | NULL
  | LISTV
  | ISEMPTY
  | HEAD
  | TAIL
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)
  | CHARV of (char)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
