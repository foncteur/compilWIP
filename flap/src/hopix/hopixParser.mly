%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF
%token EQUAL UNDERSCORE QUOTE COLON COMMA ARROW STAR
%token LPAREN RPAREN LBRACKET RBRACKET LT GT
%token FUN AND
%token<string> IDLOW IDUP


%start<HopixAST.t> program

%%

program: d=located(definition) EOF
{
   [d]
}


definition: v=vdefinition
{
  DefineValue v
}

vdefinition:
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
  TId x
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
