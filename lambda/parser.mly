
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
%token LIST
%token STRING
%token CHAR
%token FIX
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token LCURLY
%token RCURLY
%token LTRIFORCE
%token RTRIFORCE
%token COMMA
%token DOT
%token EQ
%token AS
%token COLON
%token ARROW
%token EOF
%token LIST
%token NULL
%token LISTV
%token ISEMPTY
%token HEAD
%token TAIL
%token TYPE
%token CASE
%token OF
%token PIPE
%token FATARROW

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV
%token <char> CHARV

%start s
%type <Lambda.command> s

%%

s :
    IDV EQ term EOF
      { BindTm ($1, $3) }
  | IDT EQ ty EOF
      { BindTy ($1, $3) }   
  | TYPE ty EOF
      { EvalTy ($2) }   
  | term EOF
      { Eval $1 }
    (* Syntantic sugar *)
  | LET IDV EQ term EOF
      { BindTm ($2, TmLetIn ($2, $4, TmVar $2)) } 
  | LETREC IDV COLON ty EQ term EOF   
      { BindTm ($2, TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), TmVar $2)) }     

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
  | CASE term OF caseTerm
      { TmCase ($2, $4) }    

caseTerm :
    LTRIFORCE IDV EQ IDV RTRIFORCE FATARROW term 
    {[($2, $4, $7)]}
  | LTRIFORCE IDV EQ IDV RTRIFORCE FATARROW term PIPE caseTerm
    {($2, $4, $7) :: $3}  
         
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
  | LTRIFORCE IDV EQ term RTRIFORCE AS IDT
      { TmLabel ($2, $4, $7)}            
  | LISTV LSQUARE ty RSQUARE pathTerm pathTerm
     { TmList ($3,$5,$6) }
  | ISEMPTY LSQUARE ty RSQUARE pathTerm
     { TmIsEmpty ($3,$5) }
  | HEAD LSQUARE ty RSQUARE pathTerm
     { TmHead ($3,$5) }
  | TAIL LSQUARE ty RSQUARE pathTerm
     { TmTail ($3,$5) }
  | NULL LSQUARE ty RSQUARE
     { TmEmptyList ($3) }    


pathTerm :
    pathTerm DOT IDV
    { TmProj ($1, $3) }
  | pathTerm DOT STRINGV
    { TmProj ($1, $3) }
  | pathTerm DOT INTV 
    { TmProj ($1, string_of_int $3) }
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
      { TmChar $1 }
  | LCURLY tupleFields RCURLY
      { TmTuple $2 }
  | LCURLY recordFields RCURLY
      { TmRecord $2 }
  | LCURLY RCURLY
      {TmRecord []}

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
  | LCURLY tupleTypes RCURLY
      { TyTuple ($2) }   
  | LCURLY RCURLY
      { TyRecord ([]) }
  | LCURLY recordTypes RCURLY
      { TyRecord ($2) }
  | LSQUARE ty RSQUARE
      { TyList ($2) }
  | LTRIFORCE variantTypes RTRIFORCE
      { TyVariant ($2) }
  | IDT
      { TyCustom ($1)}

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

variantTypes :
    IDV COLON ty
        {[($1, $3)]}
  | IDV COLON ty COMMA variantTypes
        {($1, $3) :: $5}     