
%{
  open Lambda;;
%}

%token LAMBDA
%token LET
%token IN

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token EOF

%token <string> IDV

%start s
%type <Lambda.term> s

%%

s :
    term EOF
      { $1 }

term :
    appTerm
      { $1 }
  | LAMBDA IDV DOT term
      { TmAbs ($2, $4) }
  | LET IDV EQ term IN term
      { TmApp (TmAbs ($2, $6), $4) }

appTerm :
    atomicTerm
      { $1 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | IDV
      { TmVar $1 }

