%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF
%token EQUAL UNDERSCORE QUOTE COLON SEMICOLON COMMA ARROW STAR BAR DOT BACKSLASH
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE LT GT
%token FUN AND TYPE EXTERN LET SWITCH IF ELSE REF
%token WHILE FOR IN TO
%token PLUS MINUS DIVIDE ASSIGN ANDOP OR READ
%token ISEQUAL ISLEQ ISGEQ ISLT ISGT
%token<string> IDLOW IDUP INT STRING CHAR


%start<HopixAST.t> program

%%

program: l=list(located(definition)) EOF
{
   l
}


definition:
| TYPE x=located(type_con)
{
  DefineType(x, [], Abstract)
}
| TYPE x=located(type_con) EQUAL t=tdefinition
{
  DefineType(x, [], t)
}
| TYPE x=located(type_con) LT l=separated_nonempty_list(COMMA, located(type_variable)) GT
{
  DefineType(x, l, Abstract)
}
| TYPE x=located(type_con) LT l=separated_nonempty_list(COMMA, located(type_variable)) GT EQUAL t=tdefinition
{
  DefineType(x, l, t)
}
| EXTERN x=located(identifier) COLON t=located(type_scheme)
{
  DeclareExtern(x,t)
}
|v=vdefinition
{
  DefineValue v
}

sum_elem_tdefinition:
| x=located(constructor)
{
  (x, [])
}
| x=located(constructor) LPAREN l=separated_nonempty_list(COMMA, located(ty)) RPAREN
{
  (x, l)
}

label:
| x=IDLOW
{
  LId x
}

record_elem_tdefinition:
| x=located(label) COLON t=located(ty)
{
  (x,t)
}

tdefinition:
| option(BAR) l=separated_nonempty_list(BAR, sum_elem_tdefinition)
{
  DefineSumType l
}
| LBRACE l=separated_nonempty_list(COMMA, record_elem_tdefinition) RBRACE
{
  DefineRecordType l
}

vdefinition:
| LET x=located(identifier) t=option(preceded(COLON, located(type_scheme))) EQUAL e=located(expr1)
{
  SimpleValue(x,t,e)
}
| FUN l=separated_nonempty_list(AND, fundef)
{
  RecFunctions l
}

identifier:
| x=IDLOW
{
  Id x
}

constructor:
| x = IDUP
{
  KId x
}

type_variable:
| QUOTE x=IDLOW
{
  TId ("`"^x)
}

fundef:
| t=option(preceded(COLON, located(type_scheme))) x=located(identifier) p=located(pattern) EQUAL e=located(expr1)
{
  (x, t, FunctionDefinition (p,e))
}

type_con:
| x= IDLOW
{
  TCon x
}

ty3:
| t=type_variable
{
  TyVar t
}
| x=type_con
{
  TyCon (x, [])
}
| x=type_con LT l=separated_nonempty_list(COMMA, located(ty)) GT
{
  TyCon(x, l)
}
| LPAREN t=ty RPAREN
{
  t
}

ty2:
| t=ty3
{
  t
}
| t1=located(ty3) STAR l=separated_nonempty_list(STAR, located(ty3))
{
  TyTuple(t1::l)
}


ty:
| t=ty2
{
  t
}
| t1=located(ty2) ARROW t2=located(ty)
{
  TyArrow(t1,t2)
}


type_scheme:
| t=located(ty)
{
  ForallTy ([], t)
}
| LBRACKET l=nonempty_list(located(type_variable)) RBRACKET t=located(ty)
{
  ForallTy (l, t)
}

literal:
| i=INT
{
  LInt (Mint.of_string i)
}
| s=STRING
{
  LString (s)
}

record_elem_expr:
| l=located(label) EQUAL e=located(expr)
{
  (l, e)
}

expr5:
| LPAREN e=expr RPAREN
{
  e
}
| l=located(literal)
{
  Literal l
}
| v=located(identifier)
{
  Variable (v,None)
}
| v=located(identifier) LBRACKET l=separated_nonempty_list(COMMA, located(ty)) RBRACKET
{
  Variable(v,Some l)
}
| x=located(constructor)
{
  Tagged (x, None, [])
}
| LBRACE llabel=separated_nonempty_list(COMMA, record_elem_expr) RBRACE LBRACKET lty=separated_nonempty_list(COMMA, located(ty)) RBRACKET
{
  Record (llabel, Some lty)
}
| LPAREN e=located(expr) COMMA l=separated_nonempty_list(COMMA, located(expr)) RPAREN
{
  Tuple (e::l)
}
| LPAREN e=located(expr) COLON t=located(ty) RPAREN
{
  TypeAnnotation (e, t)
}
| e=located(expr5) DOT v=located(label)
{
  Field (e, v)
}
| SWITCH LPAREN e=located(expr) RPAREN LBRACE option(BAR) l=separated_nonempty_list(BAR, located(branch)) RBRACE
{
  Case (e, l)
}
| IF LPAREN econd=located(expr) RPAREN LBRACE ethen=located(expr) RBRACE ELSE LBRACE eelse=located(expr) RBRACE
{
  IfThenElse (econd, ethen, eelse)
}
| WHILE LPAREN econd=located(expr) RPAREN LBRACE ebody=located(expr) RBRACE
{
  While (econd, ebody)
}
| FOR v=located(identifier) IN LPAREN estart=located(expr) TO estop=located(expr) RPAREN LBRACE ebody=located(expr) RBRACE
{
  For (v, estart, estop, ebody)
}

expr4:
| e=expr5
{
  e
}
| REF e=located(expr4)
{
  Ref e
}
| READ e=located(expr4)
{
  Read e
}

expr3:
| e=expr4
{
  e
}
| e1=located(expr3) e2=located(expr4)
{
  Apply (e1, e2)
}


expr2:
| e=expr3
{
  e
}
| e1=located(expr3) ASSIGN e2=located(expr2)
{
  Assign (e1, e2)
}

expr1:
| e=expr2
{
  e
}
| BACKSLASH p=located(pattern) ARROW e=located(expr1)
{
  Fun (FunctionDefinition (p, e))
}
| v=vdefinition SEMICOLON e=located(expr)
{
  Define (v,e)
}

expr:
| e=expr1
{
  e
}
| e1=located(expr2) SEMICOLON e2=located(expr)
{
  Sequence [e1; e2]
}

branch:
| p=located(pattern) ARROW e=located(expr)
{
  Branch (p, e)
}

pattern1:
| id=located(identifier)
{
  PVariable id
}
| UNDERSCORE
{
  PWildcard
}
| LPAREN l=separated_nonempty_list(COMMA, located(pattern)) RPAREN
{
  PTuple l 
}
| l=located(literal)
{
  PLiteral l 
}


pattern:
| p=located(pattern1) COLON t=located(ty2)
{
  PTypeAnnotation (p, t)
} 
| p=pattern1
{
  p
}


%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
