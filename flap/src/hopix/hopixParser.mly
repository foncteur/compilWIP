%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF
%token EQUAL UNDERSCORE QUOTE COLON COMMA ARROW STAR BAR
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE LT GT
%token FUN AND TYPE EXTERN LET
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
| LET x=located(identifier) t=option(preceded(COLON, located(type_scheme))) EQUAL e=located(expr)
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
| t=option(preceded(COLON, located(type_scheme))) x=located(identifier) p=located(pattern) EQUAL e=located(expr)
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

expr:
| x=located(constructor)
{
  Tagged (x, None, [])
}

pattern:
| UNDERSCORE
{
  PWildcard
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
