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
  | BOOL
  | NAT
  | STRING
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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 35 "parser.ml"
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
  270 (* BOOL *);
  271 (* NAT *);
  272 (* STRING *);
  273 (* FIX *);
  274 (* LPAREN *);
  275 (* RPAREN *);
  276 (* DOT *);
  277 (* EQ *);
  278 (* COLON *);
  279 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  280 (* INTV *);
  281 (* IDV *);
  282 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\004\000\006\000\006\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\002\000\001\000\006\000\006\000\006\000\008\000\001\000\002\000\
\002\000\002\000\003\000\002\000\002\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\017\000\
\019\000\026\000\000\000\000\000\007\000\000\000\000\000\008\000\
\009\000\010\000\000\000\000\000\000\000\012\000\000\000\001\000\
\013\000\000\000\000\000\000\000\000\000\011\000\014\000\023\000\
\024\000\025\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\004\000\
\021\000\003\000\005\000\000\000\000\000\006\000"

let yydgoto = "\002\000\
\018\000\019\000\020\000\044\000\021\000\045\000"

let yysindex = "\006\000\
\002\255\000\000\233\254\000\000\000\000\002\255\014\255\014\255\
\014\255\245\254\249\254\014\255\014\255\002\255\000\000\000\000\
\000\000\000\000\024\000\014\255\000\000\009\255\028\255\000\000\
\000\000\000\000\013\255\015\255\014\255\000\000\017\255\000\000\
\000\000\007\255\002\255\002\255\007\255\000\000\000\000\000\000\
\000\000\000\000\007\255\022\255\020\255\044\255\033\255\040\255\
\035\255\002\255\007\255\002\255\002\255\002\255\000\000\000\000\
\000\000\000\000\000\000\050\255\002\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\254\255\044\000\000\000"

let yytablesize = 276
let yytable = "\023\000\
\002\000\022\000\003\000\004\000\005\000\006\000\001\000\031\000\
\007\000\008\000\009\000\010\000\011\000\027\000\012\000\004\000\
\005\000\028\000\013\000\014\000\040\000\041\000\042\000\032\000\
\043\000\015\000\016\000\017\000\046\000\047\000\034\000\014\000\
\035\000\036\000\048\000\039\000\037\000\015\000\016\000\017\000\
\049\000\050\000\051\000\056\000\053\000\058\000\059\000\060\000\
\057\000\052\000\024\000\025\000\026\000\055\000\062\000\029\000\
\030\000\020\000\020\000\020\000\054\000\061\000\000\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\002\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000"

let yycheck = "\006\000\
\000\000\025\001\001\001\002\001\003\001\004\001\001\000\014\000\
\007\001\008\001\009\001\010\001\011\001\025\001\013\001\002\001\
\003\001\025\001\017\001\018\001\014\001\015\001\016\001\000\000\
\018\001\024\001\025\001\026\001\035\000\036\000\022\001\018\001\
\005\001\021\001\037\000\019\001\022\001\024\001\025\001\026\001\
\043\000\020\001\023\001\050\000\012\001\052\000\053\000\054\000\
\051\000\006\001\007\000\008\000\009\000\019\001\061\000\012\000\
\013\000\019\001\020\001\021\001\021\001\012\001\255\255\020\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\029\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\255\255\
\255\255\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\255\255\255\255\019\001"

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
  BOOL\000\
  NAT\000\
  STRING\000\
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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 42 "parser.mly"
      ( _1 )
# 232 "parser.ml"
               : Lambda.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 46 "parser.mly"
      ( _1 )
# 239 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 48 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 248 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 50 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 257 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 52 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 266 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 54 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8) )
# 276 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 58 "parser.mly"
      ( _1 )
# 283 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 60 "parser.mly"
      ( TmSucc _2 )
# 290 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 62 "parser.mly"
      ( TmPred _2 )
# 297 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 64 "parser.mly"
      ( TmIsZero _2 )
# 304 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 66 "parser.mly"
      ( TmConcat (_2, _3) )
# 312 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 68 "parser.mly"
      ( TmFix _2 )
# 319 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 70 "parser.mly"
      ( TmApp (_1, _2) )
# 327 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 74 "parser.mly"
      ( _2 )
# 334 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
      ( TmTrue )
# 340 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
      ( TmFalse )
# 346 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
      ( TmVar _1 )
# 353 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 82 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 363 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
      ( TmString _1 )
# 370 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 91 "parser.mly"
      ( _1 )
# 377 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 93 "parser.mly"
      ( TyArr (_1, _3) )
# 385 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 97 "parser.mly"
      ( _2 )
# 392 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
      ( TyBool )
# 398 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
      ( TyNat )
# 404 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
      ( TyString )
# 410 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.term)
