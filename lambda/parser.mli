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
  | LTRIFORCE
  | RTRIFORCE
  | COMMA
  | DOT
  | EQ
  | AS
  | COLON
  | ARROW
  | EOF
  | NULL
  | LISTV
  | ISEMPTY
  | HEAD
  | TAIL
  | TYPE
  | INTV of (int)
  | IDV of (string)
  | IDT of (string)
  | STRINGV of (string)
  | CHARV of (char)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
