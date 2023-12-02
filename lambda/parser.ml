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
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)
  | CHARV of (char)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 47 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* IN *);
  269 (* CONCAT *);
  270 (* FIRST *);
  271 (* SUB *);
  272 (* BOOL *);
  273 (* NAT *);
  274 (* TUPLE *);
  275 (* RECORD *);
  276 (* LIST *);
  277 (* STRING *);
  278 (* CHAR *);
  279 (* FIX *);
  280 (* LPAREN *);
  281 (* RPAREN *);
  282 (* LSQUARE *);
  283 (* RSQUARE *);
  284 (* LCURLY *);
  285 (* RCURLY *);
  286 (* COMMA *);
  287 (* DOT *);
  288 (* EQ *);
  289 (* COLON *);
  290 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  291 (* INTV *);
  292 (* IDV *);
  293 (* STRINGV *);
  294 (* CHARV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\007\000\
\007\000\008\000\008\000\009\000\009\000\004\000\004\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\012\000\012\000\000\000"

let yylen = "\002\000\
\004\000\002\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\003\000\002\000\002\000\002\000\002\000\
\004\000\004\000\004\000\001\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\004\000\003\000\001\000\
\003\000\003\000\005\000\003\000\003\000\001\000\003\000\003\000\
\001\000\001\000\001\000\001\000\004\000\004\000\004\000\001\000\
\003\000\003\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\022\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\000\000\026\000\027\000\052\000\000\000\
\000\000\000\000\020\000\000\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\000\000\000\000\000\000\031\000\000\000\000\000\028\000\029\000\
\000\000\000\000\041\000\042\000\000\000\000\000\000\000\043\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\030\000\
\000\000\000\000\000\000\033\000\001\000\019\000\017\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\036\000\000\000\000\000\000\000\000\000\000\000\
\000\000\040\000\005\000\039\000\004\000\006\000\000\000\000\000\
\035\000\000\000\045\000\000\000\046\000\047\000\000\000\049\000\
\000\000\007\000\000\000\051\000"

let yydgoto = "\002\000\
\023\000\044\000\025\000\101\000\026\000\027\000\045\000\046\000\
\060\000\076\000\102\000\104\000"

let yysindex = "\007\000\
\106\255\000\000\222\254\000\000\000\000\144\255\219\255\219\255\
\219\255\229\254\231\254\219\255\219\255\219\255\219\255\144\255\
\063\255\189\255\000\000\249\254\000\000\000\000\000\000\026\000\
\219\255\253\254\000\000\252\254\000\000\026\255\253\254\253\254\
\253\254\000\255\002\255\100\255\253\254\253\254\253\254\008\255\
\003\255\232\254\005\255\009\255\011\255\013\255\144\255\000\000\
\253\254\012\255\254\255\144\255\144\255\254\255\253\254\000\000\
\254\255\015\255\144\255\000\000\144\255\144\255\000\000\000\000\
\045\000\243\254\000\000\000\000\021\255\022\255\027\255\000\000\
\000\000\254\255\023\255\025\255\049\255\044\255\029\255\000\000\
\254\255\232\254\032\255\000\000\000\000\000\000\000\000\000\000\
\254\255\043\255\254\255\038\255\144\255\254\255\144\255\144\255\
\144\255\000\000\000\000\045\255\050\255\028\255\036\255\053\255\
\056\255\000\000\000\000\000\000\000\000\000\000\072\255\005\255\
\000\000\254\255\000\000\254\255\000\000\000\000\144\255\000\000\
\055\255\000\000\043\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
\250\000\038\000\000\000\000\000\000\000\000\000\075\000\112\000\
\149\000\000\000\000\000\000\000\186\000\223\000\005\001\000\000\
\000\000\000\000\204\255\059\255\000\000\000\000\000\000\000\000\
\042\001\000\000\000\000\000\000\000\000\000\000\079\001\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\064\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\068\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\075\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\209\255\005\000\000\000\043\000\006\000\
\029\000\000\000\008\000\002\000"

let yytablesize = 629
let yytable = "\024\000\
\038\000\028\000\058\000\075\000\030\000\059\000\079\000\001\000\
\034\000\080\000\035\000\031\000\032\000\033\000\040\000\042\000\
\036\000\037\000\038\000\039\000\024\000\086\000\087\000\088\000\
\047\000\048\000\092\000\050\000\051\000\049\000\052\000\053\000\
\056\000\098\000\054\000\057\000\061\000\008\000\062\000\063\000\
\055\000\064\000\066\000\105\000\085\000\065\000\108\000\081\000\
\089\000\090\000\077\000\078\000\091\000\093\000\095\000\096\000\
\115\000\082\000\094\000\083\000\097\000\100\000\106\000\003\000\
\004\000\005\000\006\000\116\000\121\000\007\000\008\000\009\000\
\010\000\011\000\009\000\012\000\013\000\014\000\103\000\114\000\
\112\000\117\000\118\000\119\000\123\000\015\000\016\000\032\000\
\017\000\041\000\018\000\107\000\034\000\109\000\110\000\111\000\
\048\000\019\000\029\000\021\000\022\000\004\000\005\000\050\000\
\084\000\113\000\003\000\004\000\005\000\006\000\099\000\010\000\
\007\000\008\000\009\000\010\000\011\000\122\000\012\000\013\000\
\014\000\120\000\000\000\016\000\124\000\017\000\000\000\018\000\
\015\000\016\000\050\000\017\000\000\000\018\000\019\000\029\000\
\021\000\022\000\000\000\000\000\019\000\020\000\021\000\022\000\
\003\000\004\000\005\000\006\000\011\000\000\000\007\000\008\000\
\009\000\010\000\011\000\000\000\012\000\013\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\016\000\
\000\000\017\000\000\000\018\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\029\000\021\000\022\000\000\000\000\000\
\000\000\014\000\000\000\000\000\000\000\003\000\004\000\005\000\
\006\000\000\000\000\000\007\000\008\000\009\000\010\000\011\000\
\000\000\012\000\013\000\014\000\000\000\024\000\024\000\000\000\
\000\000\000\000\000\000\015\000\016\000\000\000\017\000\000\000\
\018\000\000\000\000\000\000\000\004\000\005\000\015\000\019\000\
\043\000\021\000\022\000\024\000\000\000\024\000\000\000\024\000\
\024\000\024\000\024\000\000\000\000\000\000\000\024\000\024\000\
\024\000\024\000\016\000\000\000\017\000\000\000\018\000\000\000\
\000\000\003\000\000\000\000\000\000\000\019\000\029\000\021\000\
\022\000\000\000\038\000\038\000\013\000\038\000\038\000\000\000\
\000\000\000\000\000\000\000\000\038\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\000\000\074\000\024\000\024\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\000\000\000\000\038\000\038\000\038\000\038\000\008\000\
\008\000\016\000\008\000\008\000\024\000\000\000\024\000\000\000\
\024\000\008\000\000\000\024\000\000\000\000\000\000\000\024\000\
\024\000\024\000\024\000\000\000\000\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\000\000\000\000\000\000\000\000\
\008\000\008\000\008\000\008\000\009\000\009\000\012\000\009\000\
\009\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\000\000\000\000\000\000\000\000\009\000\009\000\009\000\
\009\000\010\000\010\000\000\000\010\000\010\000\000\000\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\000\000\000\000\
\000\000\000\000\010\000\010\000\010\000\010\000\011\000\011\000\
\000\000\011\000\011\000\000\000\000\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\000\000\000\000\000\000\000\000\011\000\
\011\000\011\000\011\000\014\000\014\000\000\000\014\000\014\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\000\000\000\000\000\000\000\000\014\000\014\000\014\000\014\000\
\015\000\015\000\000\000\015\000\015\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\000\000\003\000\003\000\
\000\000\015\000\015\000\015\000\015\000\003\000\013\000\013\000\
\000\000\013\000\013\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\003\000\000\000\003\000\000\000\003\000\003\000\
\000\000\000\000\000\000\000\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\000\000\000\000\000\000\000\000\013\000\
\013\000\013\000\013\000\016\000\016\000\000\000\016\000\016\000\
\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\000\000\000\000\000\000\000\000\016\000\016\000\016\000\016\000\
\012\000\012\000\000\000\012\000\012\000\000\000\000\000\000\000\
\000\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\000\000\000\000\000\000\
\000\000\012\000\012\000\012\000\012\000"

let yycheck = "\001\000\
\000\000\036\001\027\001\051\000\006\000\030\001\054\000\001\000\
\036\001\057\000\036\001\007\000\008\000\009\000\016\000\017\000\
\012\000\013\000\014\000\015\000\000\000\035\001\036\001\037\001\
\032\001\000\000\074\000\031\001\033\001\025\000\005\001\032\001\
\025\001\081\000\033\001\033\001\032\001\000\000\030\001\029\001\
\036\000\029\001\031\001\091\000\000\000\047\000\094\000\033\001\
\028\001\028\001\052\000\053\000\026\001\031\001\006\001\012\001\
\029\001\059\000\034\001\061\000\032\001\030\001\025\001\001\001\
\002\001\003\001\004\001\032\001\116\000\007\001\008\001\009\001\
\010\001\011\001\000\000\013\001\014\001\015\001\036\001\030\001\
\036\001\029\001\027\001\012\001\030\001\023\001\024\001\029\001\
\026\001\027\001\028\001\093\000\029\001\095\000\096\000\097\000\
\029\001\035\001\036\001\037\001\038\001\002\001\003\001\029\001\
\062\000\100\000\001\001\002\001\003\001\004\001\082\000\000\000\
\007\001\008\001\009\001\010\001\011\001\119\000\013\001\014\001\
\015\001\114\000\255\255\024\001\123\000\026\001\255\255\028\001\
\023\001\024\001\031\001\026\001\255\255\028\001\035\001\036\001\
\037\001\038\001\255\255\255\255\035\001\036\001\037\001\038\001\
\001\001\002\001\003\001\004\001\000\000\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\023\001\024\001\
\255\255\026\001\255\255\028\001\255\255\255\255\255\255\255\255\
\255\255\255\255\035\001\036\001\037\001\038\001\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\255\255\013\001\014\001\015\001\255\255\002\001\003\001\255\255\
\255\255\255\255\255\255\023\001\024\001\255\255\026\001\255\255\
\028\001\255\255\255\255\255\255\002\001\003\001\000\000\035\001\
\036\001\037\001\038\001\024\001\255\255\026\001\255\255\028\001\
\029\001\030\001\031\001\255\255\255\255\255\255\035\001\036\001\
\037\001\038\001\024\001\255\255\026\001\255\255\028\001\255\255\
\255\255\000\000\255\255\255\255\255\255\035\001\036\001\037\001\
\038\001\255\255\002\001\003\001\000\000\005\001\006\001\255\255\
\255\255\255\255\255\255\255\255\012\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\002\001\003\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\255\255\255\255\035\001\036\001\037\001\038\001\002\001\
\003\001\000\000\005\001\006\001\024\001\255\255\026\001\255\255\
\028\001\012\001\255\255\031\001\255\255\255\255\255\255\035\001\
\036\001\037\001\038\001\255\255\255\255\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\255\255\255\255\255\255\255\255\
\035\001\036\001\037\001\038\001\002\001\003\001\000\000\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\255\255\255\255\255\255\255\255\035\001\036\001\037\001\
\038\001\002\001\003\001\255\255\005\001\006\001\255\255\255\255\
\255\255\255\255\255\255\012\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\255\255\255\255\
\255\255\255\255\035\001\036\001\037\001\038\001\002\001\003\001\
\255\255\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\255\255\255\255\255\255\255\255\035\001\
\036\001\037\001\038\001\002\001\003\001\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\012\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\255\255\255\255\255\255\255\255\035\001\036\001\037\001\038\001\
\002\001\003\001\255\255\005\001\006\001\255\255\255\255\255\255\
\255\255\255\255\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\255\255\005\001\006\001\
\255\255\035\001\036\001\037\001\038\001\012\001\002\001\003\001\
\255\255\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\255\255\025\001\255\255\027\001\255\255\029\001\030\001\
\255\255\255\255\255\255\255\255\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\255\255\255\255\255\255\255\255\035\001\
\036\001\037\001\038\001\002\001\003\001\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\012\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\255\255\255\255\255\255\255\255\035\001\036\001\037\001\038\001\
\002\001\003\001\255\255\005\001\006\001\255\255\255\255\255\255\
\255\255\255\255\012\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\255\255\255\255\255\255\
\255\255\035\001\036\001\037\001\038\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  CONCAT\000\
  FIRST\000\
  SUB\000\
  BOOL\000\
  NAT\000\
  TUPLE\000\
  RECORD\000\
  LIST\000\
  STRING\000\
  CHAR\000\
  FIX\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  LCURLY\000\
  RCURLY\000\
  COMMA\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  STRINGV\000\
  CHARV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 54 "parser.mly"
      ( Bind (_1, _3) )
# 389 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      ( Eval _1 )
# 396 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 60 "parser.mly"
      ( _1 )
# 403 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 62 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 412 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 64 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 421 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 66 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 430 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 68 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 440 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( _1 )
# 447 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 74 "parser.mly"
      ( TmSucc _2 )
# 454 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 76 "parser.mly"
      ( TmPred _2 )
# 461 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 78 "parser.mly"
      ( TmIsZero _2 )
# 468 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 80 "parser.mly"
      ( TmConcat (_2, _3) )
# 476 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 82 "parser.mly"
      ( TmFix _2 )
# 483 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 84 "parser.mly"
      ( TmFirst _2 )
# 490 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 86 "parser.mly"
      ( TmSub _2 )
# 497 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 88 "parser.mly"
      ( TmApp (_1, _2) )
# 505 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pathTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
    ( TmProj (_1, _4) )
# 513 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pathTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
    ( TmProj (_1, _4) )
# 521 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pathTerm) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 96 "parser.mly"
    ( TmProj (_1, string_of_int _4) )
# 529 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 98 "parser.mly"
    ( _1 )
# 536 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 102 "parser.mly"
      ( _2 )
# 543 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
      ( TmTrue )
# 549 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
      ( TmFalse )
# 555 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
      ( TmVar _1 )
# 562 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 110 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 572 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
      ( TmString _1 )
# 579 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 117 "parser.mly"
      ( TmChar _1 )
# 586 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tupleFields) in
    Obj.repr(
# 119 "parser.mly"
      ( TmTuple _2 )
# 593 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'recordFields) in
    Obj.repr(
# 121 "parser.mly"
      ( TmRecord _2 )
# 600 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 123 "parser.mly"
      ( TmEmptyList _4 )
# 607 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'l) in
    Obj.repr(
# 125 "parser.mly"
      ( TmList (_2,_3) )
# 615 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 129 "parser.mly"
    ([_1])
# 622 "parser.ml"
               : 'tupleFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleFields) in
    Obj.repr(
# 131 "parser.mly"
    (_1 :: _3)
# 630 "parser.ml"
               : 'tupleFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 135 "parser.mly"
    ( [(_1, _3)] )
# 638 "parser.ml"
               : 'recordFields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordFields) in
    Obj.repr(
# 137 "parser.mly"
    ( (_1, _3) :: _5 )
# 647 "parser.ml"
               : 'recordFields))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'l) in
    Obj.repr(
# 141 "parser.mly"
      (TmList (_2,_3))
# 655 "parser.ml"
               : 'l))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 143 "parser.mly"
      (TmEmptyList _3)
# 662 "parser.ml"
               : 'l))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 147 "parser.mly"
      ( _1 )
# 669 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 149 "parser.mly"
      ( TyArr (_1, _3) )
# 677 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 153 "parser.mly"
      ( _2 )
# 684 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parser.mly"
      ( TyBool )
# 690 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
      ( TyNat )
# 696 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser.mly"
      ( TyString )
# 702 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "parser.mly"
      ( TyChar )
# 708 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'tupleTypes) in
    Obj.repr(
# 163 "parser.mly"
      ( TyTuple (_3) )
# 715 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'recordTypes) in
    Obj.repr(
# 165 "parser.mly"
      ( TyRecord (_3) )
# 722 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 167 "parser.mly"
      ( TyList (_3) )
# 729 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 171 "parser.mly"
    ([_1])
# 736 "parser.ml"
               : 'tupleTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleTypes) in
    Obj.repr(
# 173 "parser.mly"
    (_1 :: _3)
# 744 "parser.ml"
               : 'tupleTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 177 "parser.mly"
    ([(_1, _3)])
# 752 "parser.ml"
               : 'recordTypes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordTypes) in
    Obj.repr(
# 179 "parser.mly"
    ((_1, _3) :: _5)
# 761 "parser.ml"
               : 'recordTypes))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.command)
