
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token FIRST
%token SUB
%token BOOL
%token NAT
%token TUPLE
%token RECORD
%token STRING
%token CHAR
%token FIX
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token LCURLY
%token RCURLY
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> STRINGV
%token <char> CHARV

%start s
%type <Lambda.command> s

%%

s :
    IDV EQ term EOF
      { Bind ($1, $3) }
  | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | CONCAT pathTerm pathTerm
      { TmConcat ($2, $3) } 
  | FIX pathTerm
      { TmFix $2 }
  | FIRST pathTerm
      { TmFirst $2 }
  | SUB pathTerm
      { TmSub $2 }    
  | appTerm pathTerm
      { TmApp ($1, $2) }

pathTerm :
    pathTerm DOT DOT STRINGV
    { TmProj ($1, $4) }
  | pathTerm DOT DOT INTV 
    { TmProj ($1, string_of_int $4) }
  | atomicTerm
    { $1 }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }
  | CHARV
      { TmChar $1}
  | LCURLY tupleFields RCURLY
      { TmTuple $2}
  | LCURLY recordFields RCURLY
      { TmRecord $2}

tupleFields : 
  term 
    {[$1]}
  | term COMMA tupleFields
    {$1 :: $3}      

recordFields :
    IDV EQ term
    { [($1, $3)] }
  | IDV EQ term COMMA recordFields
    { ($1, $3) :: $5 }

ty :
    atomicTy
      { $1 }   
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | CHAR
      { TyChar }  
  | TUPLE LCURLY tupleTypes RCURLY
      { TyTuple ($3)}   
  | RECORD LCURLY recordTypes RCURLY
      { TyRecord ($3)}          

tupleTypes : 
  ty 
    {[$1]}
  | ty COMMA tupleTypes
    {$1 :: $3}  

recordTypes : 
    IDV EQ ty 
    {[($1, $3)]}
  | IDV EQ ty COMMA recordTypes
    {($1, $3) :: $5}              
