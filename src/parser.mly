%{ 
	open Ast
%}

%token SPACE TAB EOL
%token ASSIGN PLUS MINUS TIMES DIVIDE MOD EXP PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODEQ EXPEQ
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE SEMI COMMA
%token EQ NEQ LT LEQ GT GEQ AND OR NOT NEG NOP
%token IF ELSE NOELSE ELIF WHILE FOR IN FUNC RETURN BREAK CONTINUE
%token IMPORT DEF ARROW COLON DOT PRINT TYPE RANGE PASS ASSERT
%token BOOL INT FLOAT STRING ARR CLASS VOID
%token <bool> BOOL_LITERAL
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <string> VARIABLE
%token INDENT DEDENT
%token EOF

%start program
%type <Ast.stmt list> program

%nonassoc NOFIELD
%nonassoc FIELD
%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN PLUSEQ MINUSEQ DIVIDEEQ TIMESEQ MODEQ
%left DOT
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right EXP EXPEQ
%right NOT NEG
%left SEMI

%nonassoc LPAREN LBRACK LBRACE
%nonassoc RPAREN RBRACK RBRACE

%%

program: stmt_list EOF { List.rev $1 }

stmt_list:
  | { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI { Expr $1 }
  | stmt SEMI { $1 }
  | IMPORT VARIABLE SEMI { Import($2) }
  | CLASS VARIABLE COLON SEMI stmt_block { Class($2, $5) }
  | DEF VARIABLE LPAREN formals_opt RPAREN COLON SEMI stmt_block { raise(Failure(Printf.sprintf "TypeError: function '%s' needs explicit type" $2))}
  | DEF VARIABLE LPAREN formals_opt RPAREN ARROW typ COLON SEMI stmt_block { Func(Bind($2, $7), $4, $10) }
  | RETURN expr SEMI { Return $2 }
  | IF expr COLON SEMI stmt_block %prec NOELSE { If($2, $5, Block([])) }
  | IF expr COLON SEMI stmt_block ELSE COLON SEMI stmt_block { If($2, $5, $9) }
  | FOR bind_opt IN expr COLON SEMI stmt_block { For($2, $4, $7) }
  | FOR bind_opt IN RANGE LPAREN expr RPAREN COLON SEMI stmt_block { Range($2, $6, $10) }
  | WHILE expr COLON SEMI stmt_block { While($2, $5) }
  | formal_asn_list ASSIGN expr { Asn(List.rev $1, $3) }
  | lvalue PLUSEQ expr { Asn([$1], Binop($1, Add, $3)) }
  | lvalue MINUSEQ expr { Asn([$1], Binop($1, Sub, $3)) }
  | lvalue TIMESEQ expr { Asn([$1], Binop($1, Mul, $3)) }
  | lvalue DIVIDEEQ expr { Asn([$1], Binop($1, Div, $3)) }
  | lvalue EXPEQ expr { Asn([$1], Binop($1, Exp, $3)) }
  | TYPE LPAREN expr RPAREN { Type($3) }
  | PRINT LPAREN expr RPAREN { Print($3) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | PASS { Nop }
  | NOP { Nop }

formal_asn_list:
  | lvalue { [$1] }
  | formal_asn_list ASSIGN lvalue { $3 :: $1 }

lvalue:
  | bind_opt { Var $1 }
  | list_access { $1 }

bind_opt:
  | VARIABLE { Bind($1, Dyn) }
  | typ VARIABLE { Bind($2, $1) }
  | typ VARIABLE LBRACK RBRACK { Bind($2, $1) }

list_access:
  | expr LBRACK expr RBRACK { ListAccess($1, $3) }
  | expr LBRACK expr COLON expr RBRACK { ListSlice($1, $3, $5) }

stmt_block: 
  | INDENT stmt_list DEDENT { Block(List.rev $2) }

formals_opt:
  | { [] }
  | formal_list { List.rev $1 }

formal_list: 
  | bind_opt { [$1] }
  | formal_list COMMA bind_opt { $3 :: $1 }

actuals_opt: 
  | { [] }
  | actuals_list { List.rev $1 }

actuals_list:
  | expr { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

typ:
  | FLOAT { Float }
  | INT { Int }
  | BOOL { Bool }
  | STRING { String }
  | ARR { Arr }
  | VOID { Dyn }
  | FUNC { FuncType }

expr:
| list_access { $1 }
| VARIABLE { Var(Bind($1, Dyn)) }
| expr LPAREN actuals_opt RPAREN { Call($1, $3) }
| expr DOT VARIABLE LPAREN actuals_opt RPAREN { Method($1, $3, $5) }
| expr DOT VARIABLE %prec FIELD { Field($1, $3) }
| MINUS expr %prec NEG { Unop(Neg, $2) }
| NOT expr %prec NOT { Unop(Not, $2) }
| LPAREN expr RPAREN { $2 }
| FLOAT_LITERAL { Lit(FloatLit($1)) }
| BOOL_LITERAL { Lit(BoolLit($1)) }
| INT_LITERAL { Lit(IntLit($1)) }
| STRING_LITERAL { Lit(StringLit($1)) }
| LBRACK actuals_opt RBRACK { List($2) }
| expr EQ expr { Binop($1, Eq, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr { Binop($1, Less, $3) }
| expr GT expr { Binop($1, Greater, $3) }
| expr LEQ expr { Binop($1, Leq, $3) }
| expr GEQ expr { Binop($1, Geq, $3) }
| expr AND expr { Binop($1, And, $3) }
| expr OR expr { Binop($1, Or, $3) }
| expr PLUS expr { Binop($1, Add, $3) }
| expr MINUS expr { Binop($1, Sub, $3) }
| expr TIMES expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr EXP expr { Binop($1, Exp, $3) }
| typ LPAREN expr RPAREN { Cast($1, $3) }
