%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position


%}

%token EOF
%token EQUAL UNDERSCORE
%token FUN
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
| FUN f = fundef
{
  RecFunctions [f]
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

fundef:
| x=located(identifier) p=located(pattern) EQUAL e=located(expr)
{
  (x, None, FunctionDefinition (p,e))
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
