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
  | STRINGV of (string)
  | CHARV of (char)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 39 "parser.ml"
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
  285 (* STRINGV *);
  286 (* CHARV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\002\000\002\000\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\004\000\004\000\002\000\001\000\006\000\006\000\006\000\008\000\
\001\000\002\000\002\000\002\000\003\000\002\000\002\000\002\000\
\002\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\019\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\023\000\024\000\032\000\000\000\000\000\009\000\
\000\000\021\000\000\000\010\000\011\000\012\000\000\000\000\000\
\000\000\015\000\016\000\014\000\000\000\000\000\003\000\017\000\
\000\000\000\000\000\000\000\000\013\000\018\000\028\000\029\000\
\030\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\002\000\000\000\000\000\
\000\000\000\000\000\000\027\000\026\000\006\000\005\000\007\000\
\000\000\000\000\008\000"

let yydgoto = "\002\000\
\021\000\060\000\037\000\023\000\024\000\054\000"

let yysindex = "\255\255\
\051\255\000\000\231\254\000\000\000\000\081\255\095\255\095\255\
\095\255\234\254\235\254\095\255\095\255\095\255\095\255\081\255\
\000\000\240\254\000\000\000\000\000\000\014\000\095\255\000\000\
\246\254\000\000\016\255\000\000\000\000\000\000\007\255\008\255\
\095\255\000\000\000\000\000\000\010\255\009\255\000\000\000\000\
\118\255\081\255\081\255\118\255\000\000\000\000\000\000\000\000\
\000\000\000\000\009\255\034\000\035\000\014\255\118\255\019\255\
\037\255\032\255\023\255\026\255\000\000\000\000\118\255\081\255\
\081\255\081\255\081\255\000\000\000\000\000\000\000\000\000\000\
\038\255\081\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\088\000\003\000\000\000\105\000\000\000"

let yytablesize = 287
let yytable = "\001\000\
\021\000\025\000\025\000\022\000\004\000\031\000\032\000\038\000\
\027\000\003\000\004\000\005\000\006\000\039\000\041\000\007\000\
\008\000\009\000\010\000\011\000\042\000\012\000\013\000\014\000\
\047\000\048\000\049\000\050\000\015\000\051\000\043\000\046\000\
\044\000\061\000\062\000\017\000\026\000\019\000\020\000\063\000\
\053\000\064\000\065\000\066\000\057\000\058\000\067\000\068\000\
\000\000\074\000\000\000\003\000\004\000\005\000\006\000\000\000\
\000\000\007\000\008\000\009\000\010\000\011\000\000\000\012\000\
\013\000\014\000\070\000\071\000\072\000\073\000\015\000\016\000\
\000\000\000\000\000\000\000\000\075\000\017\000\018\000\019\000\
\020\000\003\000\004\000\005\000\006\000\000\000\000\000\007\000\
\008\000\009\000\010\000\011\000\000\000\012\000\013\000\014\000\
\004\000\005\000\000\000\000\000\015\000\016\000\000\000\000\000\
\000\000\000\000\000\000\017\000\026\000\019\000\020\000\028\000\
\029\000\030\000\000\000\016\000\033\000\034\000\035\000\036\000\
\000\000\017\000\026\000\019\000\020\000\052\000\000\000\040\000\
\056\000\000\000\000\000\059\000\000\000\047\000\048\000\049\000\
\050\000\045\000\055\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\069\000\000\000\
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
\025\000\025\000\004\000\021\000\021\000\021\000\021\000"

let yycheck = "\001\000\
\000\000\000\000\028\001\001\000\000\000\028\001\028\001\024\001\
\006\000\001\001\002\001\003\001\004\001\000\000\025\001\007\001\
\008\001\009\001\010\001\011\001\005\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\024\001\022\001\
\025\001\000\000\000\000\027\001\028\001\029\001\030\001\026\001\
\038\000\023\001\006\001\012\001\042\000\043\000\024\001\022\001\
\255\255\012\001\255\255\001\001\002\001\003\001\004\001\255\255\
\255\255\007\001\008\001\009\001\010\001\011\001\255\255\013\001\
\014\001\015\001\064\000\065\000\066\000\067\000\020\001\021\001\
\255\255\255\255\255\255\255\255\074\000\027\001\028\001\029\001\
\030\001\001\001\002\001\003\001\004\001\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\015\001\
\002\001\003\001\255\255\255\255\020\001\021\001\255\255\255\255\
\255\255\255\255\255\255\027\001\028\001\029\001\030\001\007\000\
\008\000\009\000\255\255\021\001\012\000\013\000\014\000\015\000\
\255\255\027\001\028\001\029\001\030\001\038\000\255\255\023\000\
\041\000\255\255\255\255\044\000\255\255\016\001\017\001\018\001\
\019\001\033\000\021\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\063\000\255\255\
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
\023\001\024\001\022\001\027\001\028\001\029\001\030\001"

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
  STRINGV\000\
  CHARV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 46 "parser.mly"
      ( BindT (_1, _3) )
# 253 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 48 "parser.mly"
      ( Bind (_1, _3) )
# 261 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 50 "parser.mly"
      ( Eval _1 )
# 268 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 54 "parser.mly"
      ( _1 )
# 275 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 284 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 58 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 293 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 60 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 302 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 62 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 312 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 66 "parser.mly"
      ( _1 )
# 319 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 68 "parser.mly"
      ( TmSucc _2 )
# 326 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 70 "parser.mly"
      ( TmPred _2 )
# 333 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( TmIsZero _2 )
# 340 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 74 "parser.mly"
      ( TmConcat (_2, _3) )
# 348 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 76 "parser.mly"
      ( TmFix _2 )
# 355 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 78 "parser.mly"
      ( TmFirst _2 )
# 362 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 80 "parser.mly"
      ( TmSub _2 )
# 369 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 82 "parser.mly"
      ( TmApp (_1, _2) )
# 377 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 86 "parser.mly"
      ( _2 )
# 384 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
      ( TmTrue )
# 390 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
      ( TmFalse )
# 396 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
      ( TmVar _1 )
# 403 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 413 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
      ( TmString _1 )
# 420 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 101 "parser.mly"
      ( TmChar _1)
# 427 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 105 "parser.mly"
      ( _1 )
# 434 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 107 "parser.mly"
      ( TyArr (_1, _3) )
# 442 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 111 "parser.mly"
      ( _2 )
# 449 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
      ( TyBool )
# 455 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
      ( TyNat )
# 461 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
      ( TyString )
# 467 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
      ( TyChar )
# 473 "parser.ml"
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
