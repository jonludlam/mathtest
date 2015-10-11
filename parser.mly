%{
  open Maths

%}

%token <float> NUM
%token <string> VAR
%token <string> FUNC
%token PLUS MINUS MULTIPLY DIVIDE 
%token LPAREN RPAREN
%token EQUALS COMMA
%token EOF
%token <string> LABEL 

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG

%start expreof
%start eqneof
%type <Maths.Expression.expr> expreof
%type <Maths.Equation.eqn> eqneof
%type <Maths.Expression.expr list> exprseq
%%

expreof : expr EOF { $1 }

eqneof : eqn EOF { $1 }

eqn : expr EQUALS expr { ($1,$3) }

exprseq:   /* empty */	{[]}
        | exprseq1		{$1}
        ;

exprseq1:  expr			{[$1]}
        | exprseq1 COMMA expr		{$1 @ [$3]}
        ;

expr : NUM { Maths.Expression.Const($1) }
| VAR { Maths.Expression.Variable($1) }
| expr PLUS expr { Maths.Expression.Mop(Maths.Expression.Add,[$1;$3]) }
| expr MINUS expr { Maths.Expression.Binop(Maths.Expression.Sub,$1,$3) }
| expr expr %prec MULTIPLY { Maths.Expression.Mop(Maths.Expression.Mul,[$1;$2]) }
| expr MULTIPLY expr { Maths.Expression.Mop(Maths.Expression.Mul,[$1;$3]) }
| expr DIVIDE expr { Maths.Expression.Binop(Maths.Expression.Div,$1,$3) }
| MINUS expr %prec NEG { Maths.Expression.Binop(Maths.Expression.Sub, Maths.Expression.Const 0.0, $2) }
| LPAREN expr RPAREN { $2 }
| FUNC LPAREN exprseq RPAREN {
    match $1 with
      | "\\deriv" -> let [x;y]=$3 in Maths.Expression.Binop(Maths.Expression.Deriv,x,y)
      | _ -> failwith "Unknown function" }
;
%%
