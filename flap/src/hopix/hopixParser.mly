%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position

  let mkbinop e1 p e2 name =
    let pos = Position.position p in
    let pos2 = Position.join (Position.position e1) pos in
    Apply(Position.with_pos pos2 (Apply (Position.with_pos pos (Variable (Position.with_pos pos (Id ("`" ^ name ^ "`")), None)), e1)), e2)

%}

%token EOF
%token EQUAL UNDERSCORE COLON SEMICOLON COMMA ARROW STAR BAR DOT BACKSLASH AMPERSAND
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE LT GT
%token FUN AND TYPE EXTERN LET SWITCH IF ELSE REF
%token WHILE DO FOR IN TO
%token PLUS MINUS DIVIDE ASSIGN ANDOP OROP READ
%token ISEQUAL ISLEQ ISGEQ ISLT ISGT
%token<char> CHAR
%token<string> IDLOW IDUP IDQUOTE INT STRING

%start<HopixAST.t> program

%nonassoc CONSTR
%nonassoc LPAREN

%%

program:
| l=list(located(definition)) EOF
{
   l
}
| error
{
  Error.error "parsing" (Position.lex_join $startpos $endpos) "Syntax error."
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
| v=vdefinition(expr)
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

vdefinition(EXPR):
| LET x=located(identifier) t=option(preceded(COLON, located(type_scheme))) EQUAL e=located(EXPR)
{
  SimpleValue(x,t,e)
}
| FUN l=separated_nonempty_list(AND, fundef(EXPR))
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
| x=IDQUOTE
{
  TId x
}

fundef(EXPR):
| t=option(preceded(COLON, located(type_scheme))) x=located(identifier) p=located(pattern) EQUAL e=located(EXPR)
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
| c=CHAR
{
  LChar (c)
}

record_elem_expr:
| l=located(label) EQUAL e=located(expr1)
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
| v=located(identifier) LT l=separated_list(COMMA, located(ty)) GT
{
  Variable(v,Some l)
}
| x=located(constructor) %prec CONSTR
{
  Tagged (x, None, [])
}
| x=located(constructor) LT tys=separated_list(COMMA, located(ty)) GT %prec CONSTR
{
  Tagged (x, Some tys, [])
}
| x=located(constructor) LPAREN es=separated_nonempty_list(COMMA, located(expr)) RPAREN
{
  Tagged (x, None, es)
}
| x=located(constructor) LT tys=separated_list(COMMA, located(ty)) GT LPAREN es=separated_nonempty_list(COMMA, located(expr)) RPAREN
{
  Tagged (x, Some tys, es)
}
| LBRACE llabel=separated_nonempty_list(COMMA, record_elem_expr) RBRACE
{
  Record (llabel, None)
}
| LBRACE llabel=separated_nonempty_list(COMMA, record_elem_expr) RBRACE LT lty=separated_nonempty_list(COMMA, located(ty)) GT
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
| DO LBRACE ebody=located(expr) RBRACE WHILE LPAREN econd=located(expr) RPAREN
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

expr2c:
| e=expr3
{
  e
}

expr2b:
| e=expr2c
{
  e
}
| e1=located(expr2b) p=located(STAR) e2=located(expr2c)
{
  mkbinop e1 p e2 "*"
}
| e1=located(expr2b) p=located(DIVIDE) e2=located(expr2c)
{
  mkbinop e1 p e2 "/"
}

expr2a:
| e=expr2b
{
  e
}
| e1=located(expr2a) p=located(PLUS) e2=located(expr2b)
{
  mkbinop e1 p e2 "+"
}
| e1=located(expr2a) p=located(MINUS) e2=located(expr2b)
{
  mkbinop e1 p e2 "-"
}

expr2ac:
| e=expr2a
{
  e
}
| e1=located(expr2ac) p=located(ANDOP) e2=located(expr2a)
{
  mkbinop e1 p e2 "&&"
}

expr2ab:
| e=expr2ac
{
  e
}
| e1=located(expr2ab) p=located(OROP) e2=located(expr2ac)
{
  mkbinop e1 p e2 "||"
}


expr2aa:
| e=expr2ab
{
  e
}
| e1=located(expr2ab) p=located(ISEQUAL) e2=located(expr2ab)
{
  mkbinop e1 p e2 "=?"
}
| e1=located(expr2ab) p=located(ISLEQ) e2=located(expr2ab)
{
  mkbinop e1 p e2 "<=?"
}
| e1=located(expr2ab) p=located(ISGEQ) e2=located(expr2ab)
{
  mkbinop e1 p e2 ">=?"
}
| e1=located(expr2ab) p=located(ISLT) e2=located(expr2ab)
{
  mkbinop e1 p e2 "<?"
}
| e1=located(expr2ab) p=located(ISGT) e2=located(expr2ab)
{
  mkbinop e1 p e2 ">?"
}


expr2:
| e=expr2aa
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

expr:
| e=expr1
{
  e
}
| e1=located(expr1) SEMICOLON e2=located(expr)
{
  Sequence [e1; e2]
}
| v=vdefinition(expr1) SEMICOLON e=located(expr)
{
  Define (v,e)
}

branch:
| p=located(pattern) ARROW e=located(expr)
{
  Branch (p, e)
}

record_elem_pattern:
| l=located(label) EQUAL p=located(pattern)
{
  (l, p)
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
| LPAREN p=pattern RPAREN
{
  p
}
| LPAREN p=located(pattern) COMMA l=separated_nonempty_list(COMMA, located(pattern)) RPAREN
{
  PTuple (p :: l)
}
| l=located(literal)
{
  PLiteral l
}
| c=located(constructor)
{
  PTaggedValue (c, None, [])
}
| c=located(constructor) LT tys=separated_nonempty_list(COMMA, located(ty)) GT
{
  PTaggedValue (c, Some tys, [])
}
| c=located(constructor) LPAREN ps=separated_nonempty_list(COMMA, located(pattern)) RPAREN
{
  PTaggedValue (c, None, ps)
}
| c=located(constructor) LT tys=separated_nonempty_list(COMMA, located(ty)) GT LPAREN ps=separated_nonempty_list(COMMA, located(pattern)) RPAREN
{
  PTaggedValue (c, Some tys, ps)
}
| LBRACE l=separated_nonempty_list(COMMA, record_elem_pattern) RBRACE
{
  PRecord (l, None)
}
| LBRACE l=separated_nonempty_list(COMMA, record_elem_pattern) RBRACE LT tys=separated_nonempty_list(COMMA, located(ty)) GT
{
  PRecord (l, Some tys)
}

pattern:
| p=located(pattern1) COLON t=located(ty2)
{
  PTypeAnnotation (p, t)
}
| p1=located(pattern1) BAR p2=separated_nonempty_list(BAR, located(pattern1))
{
  POr (p1 :: p2)
}
| p1=located(pattern1) AMPERSAND p2=separated_nonempty_list(AMPERSAND, located(pattern1))
{
  PAnd (p1 :: p2)
}
| p=pattern1
{
  p
}


%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
