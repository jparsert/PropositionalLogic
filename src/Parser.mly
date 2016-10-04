%{
        open Formula
%}


%token <bool> CONST 
%token <string> VAR
%token NOT 
%token AND
%token OR
%token IMP
%token LPAREN
%token RPAREN
%token EOF

%right IMP
%left OR
%left AND
%right NOT

%start <Formula.t> prog 
%%

prog: 
        | e = expr; EOF{ e }

expr:
        | cnst = CONST { Const cnst }
        | v = VAR { Var v }
        | NOT; e1 = expr { Not e1 }
        | e1 = expr; AND; e2 = expr { And ( e1 , e2) } 
        | e1 = expr; OR; e2 = expr { Or ( e1 , e2) } 
        | e1 = expr; IMP; e2 = expr { Imp ( e1 , e2) }
        | LPAREN; e = expr; RPAREN { e }
        ;
