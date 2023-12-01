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
  | STRING
  | CHAR
  | FIX
  | LPAREN
  | RPAREN
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | IDV of (string)
  | IDT of (string)
  | STRINGV of (string)
  | CHARV of (char)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 40 "parser.ml"
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
  274 (* STRING *);
  275 (* CHAR *);
  276 (* FIX *);
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* DOT *);
  280 (* EQ *);
  281 (* COLON *);
  282 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  283 (* INTV *);
  284 (* IDV *);
  285 (* IDT *);
  286 (* STRINGV *);
  287 (* CHARV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\002\000\002\000\002\000\006\000\006\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\004\000\004\000\002\000\001\000\006\000\006\000\006\000\008\000\
\001\000\002\000\002\000\002\000\003\000\002\000\002\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\001\000\003\000\001\000\001\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\019\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\000\000\023\000\024\000\033\000\000\000\000\000\
\009\000\000\000\021\000\000\000\010\000\011\000\012\000\000\000\
\000\000\000\000\015\000\016\000\014\000\000\000\000\000\000\000\
\003\000\017\000\000\000\000\000\000\000\000\000\013\000\018\000\
\000\000\029\000\030\000\031\000\032\000\000\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\028\000\026\000\006\000\
\005\000\007\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\022\000\056\000\023\000\024\000\025\000\057\000"

let yysindex = "\003\000\
\014\255\000\000\231\254\000\000\000\000\064\255\028\255\028\255\
\028\255\240\254\241\254\028\255\028\255\028\255\028\255\064\255\
\000\000\252\254\002\255\000\000\000\000\000\000\014\000\028\255\
\000\000\007\255\000\000\031\255\000\000\000\000\000\000\013\255\
\015\255\028\255\000\000\000\000\000\000\024\255\064\255\246\254\
\000\000\000\000\246\254\064\255\064\255\246\254\000\000\000\000\
\048\000\000\000\000\000\000\000\000\000\246\254\000\000\050\000\
\021\255\029\255\045\255\041\255\030\255\000\000\035\255\000\000\
\246\254\064\255\064\255\064\255\064\255\000\000\000\000\000\000\
\000\000\000\000\052\255\064\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\047\000\250\255\000\000\074\000\000\000"

let yytablesize = 288
let yytable = "\028\000\
\021\000\025\000\026\000\001\000\004\000\050\000\051\000\052\000\
\053\000\038\000\054\000\032\000\033\000\041\000\003\000\004\000\
\005\000\006\000\055\000\039\000\007\000\008\000\009\000\010\000\
\011\000\040\000\012\000\013\000\014\000\004\000\005\000\043\000\
\049\000\015\000\016\000\044\000\045\000\059\000\060\000\046\000\
\017\000\018\000\019\000\020\000\021\000\048\000\065\000\062\000\
\016\000\064\000\067\000\066\000\068\000\069\000\017\000\027\000\
\070\000\020\000\021\000\072\000\073\000\074\000\075\000\076\000\
\003\000\004\000\005\000\006\000\000\000\077\000\007\000\008\000\
\009\000\010\000\011\000\000\000\012\000\013\000\014\000\000\000\
\029\000\030\000\031\000\015\000\016\000\034\000\035\000\036\000\
\037\000\058\000\017\000\027\000\061\000\020\000\021\000\000\000\
\000\000\042\000\000\000\000\000\063\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\000\000\000\000\000\000\071\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\021\000\000\000\000\000\000\000\000\000\
\000\000\004\000\004\000\000\000\000\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\021\000\000\000\025\000\
\025\000\025\000\004\000\021\000\021\000\000\000\021\000\021\000"

let yycheck = "\006\000\
\000\000\000\000\028\001\001\000\000\000\016\001\017\001\018\001\
\019\001\016\000\021\001\028\001\028\001\000\000\001\001\002\001\
\003\001\004\001\029\001\024\001\007\001\008\001\009\001\010\001\
\011\001\024\001\013\001\014\001\015\001\002\001\003\001\025\001\
\039\000\020\001\021\001\005\001\024\001\044\000\045\000\025\001\
\027\001\028\001\029\001\030\001\031\001\022\001\026\001\000\000\
\021\001\000\000\006\001\023\001\012\001\024\001\027\001\028\001\
\022\001\030\001\031\001\066\000\067\000\068\000\069\000\012\001\
\001\001\002\001\003\001\004\001\255\255\076\000\007\001\008\001\
\009\001\010\001\011\001\255\255\013\001\014\001\015\001\255\255\
\007\000\008\000\009\000\020\001\021\001\012\000\013\000\014\000\
\015\000\043\000\027\001\028\001\046\000\030\001\031\001\255\255\
\255\255\024\000\255\255\255\255\054\000\255\255\255\255\255\255\
\255\255\255\255\255\255\034\000\255\255\255\255\255\255\065\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\255\255\255\255\
\255\255\005\001\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\255\255\255\255\255\255\255\255\021\001\255\255\022\001\
\023\001\024\001\022\001\027\001\028\001\255\255\030\001\031\001"

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
  STRING\000\
  CHAR\000\
  FIX\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  IDT\000\
  STRINGV\000\
  CHARV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 47 "parser.mly"
      ( BindTy (_1, _3) )
# 258 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 49 "parser.mly"
      ( Bind (_1, _3) )
# 266 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 51 "parser.mly"
      ( Eval _1 )
# 273 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 55 "parser.mly"
      ( _1 )
# 280 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 57 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 289 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 298 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 307 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 317 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( _1 )
# 324 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( TmSucc _2 )
# 331 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmPred _2 )
# 338 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( TmIsZero _2 )
# 345 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmConcat (_2, _3) )
# 353 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmFix _2 )
# 360 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      ( TmFirst _2 )
# 367 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 81 "parser.mly"
      ( TmSub _2 )
# 374 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 83 "parser.mly"
      ( TmApp (_1, _2) )
# 382 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 87 "parser.mly"
      ( _2 )
# 389 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
      ( TmTrue )
# 395 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
      ( TmFalse )
# 401 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
      ( TmVar _1 )
# 408 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 418 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
      ( TmString _1 )
# 425 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 102 "parser.mly"
      ( TmChar _1)
# 432 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 106 "parser.mly"
      ( _1 )
# 439 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 108 "parser.mly"
      ( TyArr (_1, _3) )
# 447 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
      ( TyVarTy (_1))
# 454 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 114 "parser.mly"
      ( _2 )
# 461 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
      ( TyBool )
# 467 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
      ( TyNat )
# 473 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
      ( TyString )
# 479 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
      ( TyChar )
# 485 "parser.ml"
               : 'atomicTy))
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
