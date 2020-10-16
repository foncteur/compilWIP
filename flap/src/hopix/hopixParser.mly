%{ (* -*- tuareg -*- *)

  open HopixAST
  open Position

  (* La fonction mkbinop e1 p e2 name sert à écrire l'AST pour que si name est la chaine string pour un opérateur op alors 
    l'AST est de la forme Apply ( Apply(op, e1), e2 ) *)

  let mkbinop e1 p e2 name =
    let pos = Position.position p in
    let pos2 = Position.join (Position.position e1) pos in
    Apply (
      Position.with_pos pos2 (Apply (
        Position.with_pos pos (
          Variable (Position.with_pos pos (Id ("`" ^ name ^ "`")), None)
        ), 
        e1)), 
      e2
    )

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

(* tyNonAmbiguous gère les cas des définitions de types non ambigues *)
tyNonAmbiguous:
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

(* tyTuples gère le cas des définitions de types de tuples *)
tyTuples:
| t=tyNonAmbiguous
{
  t
}
| t1=located(tyNonAmbiguous) STAR l=separated_nonempty_list(STAR, located(tyNonAmbiguous))
{
  TyTuple(t1::l)
}

(* ty gère le cas des définitions de types de fonctions *)
ty:
| t=tyTuples
{
  t
}
| t1=located(tyTuples) ARROW t2=located(ty)
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

(* record_elem_expr sert à parser la brique élémentaire des records de la forme l_i = e_i.
Attention, on n'autorise pas de séquences d'expressions dans ce cas là, on passe donc directement à exprAnonymFun *)
record_elem_expr:
| l=located(label) EQUAL e=located(exprAnonymFun)
{
  (l, e)
}

(* exprNonAmbiguous gère tout les cas d'expressions délimitées sans ambiguïté *)
exprNonAmbiguous:
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
| e=located(exprNonAmbiguous) DOT v=located(label)
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
  Sequence [ebody ; Position.with_poss $startpos $endpos (While (econd, ebody))]
}
| FOR v=located(identifier) IN LPAREN estart=located(expr) TO estop=located(expr) RPAREN LBRACE ebody=located(expr) RBRACE
{
  For (v, estart, estop, ebody)
}

(* exprRefRead gère le cas des références du point de vue de leur définition (ref e) ou de leur lecture (!e) *)
exprRefRead:
| e=exprNonAmbiguous
{
  e
}
| REF e=located(exprRefRead)
{
  Ref e
}
| READ e=located(exprRefRead)
{
  Read e
}

(* exprApplyFun gère le cas d'applications de fonctions *)
exprApplyFun:
| e=exprRefRead
{
  e
}
| e1=located(exprApplyFun) e2=located(exprRefRead)
{
  Apply (e1, e2)
}

(* exprStarDivideOps gère le cas d'expressions ayant un opérateur binaire * ou / *)
exprStarDivideOps:
| e=exprApplyFun
{
  e
}
| e1=located(exprStarDivideOps) p=located(STAR) e2=located(exprApplyFun)
{
  mkbinop e1 p e2 "*"
}
| e1=located(exprStarDivideOps) p=located(DIVIDE) e2=located(exprApplyFun)
{
  mkbinop e1 p e2 "/"
}

(* exprPlusMinusOps gère le cas des expressions ayant un opérateur binaire - ou + qui sont tout les deux moins prioritaire
devant la multiplication ou la division, on les places donc en premier *)
exprPlusMinusOps:
| e=exprStarDivideOps
{
  e
}
| e1=located(exprPlusMinusOps) p=located(PLUS) e2=located(exprStarDivideOps)
{
  mkbinop e1 p e2 "+"
}
| e1=located(exprPlusMinusOps) p=located(MINUS) e2=located(exprStarDivideOps)
{
  mkbinop e1 p e2 "-"
}

(* exprAndOp gère le cas des expressions ayant un opérateur binaire ANDOP.
Comme ANDOP est plus prioritaire que OROP, on le met en second *)
exprAndOp:
| e=exprPlusMinusOps
{
  e
}
| e1=located(exprAndOp) p=located(ANDOP) e2=located(exprPlusMinusOps)
{
  mkbinop e1 p e2 "&&"
}

(* exprOrOp gère le cas des expressions ayant un opérateur binaire OROP.
Attention : l'opérateur binaire OROP est moins prioritaire que ANDOP, on le met alors en premier *)
exprOrOp:
| e=exprAndOp
{
  e
}
| e1=located(exprOrOp) p=located(OROP) e2=located(exprAndOp)
{
  mkbinop e1 p e2 "||"
}

(* exprCompBinOp gère le cas des expressions ayant un opérateur binaire de comparaison *)
exprCompBinOp:
| e=exprOrOp
{
  e
}
| e1=located(exprOrOp) p=located(ISEQUAL) e2=located(exprPlusMinusOps)
{
  mkbinop e1 p e2 "=?"
}
| e1=located(exprOrOp) p=located(ISLEQ) e2=located(exprPlusMinusOps)
{
  mkbinop e1 p e2 "<=?"
}
| e1=located(exprOrOp) p=located(ISGEQ) e2=located(exprPlusMinusOps)
{
  mkbinop e1 p e2 ">=?"
}
| e1=located(exprOrOp) p=located(ISLT) e2=located(exprPlusMinusOps)
{
  mkbinop e1 p e2 "<?"
}
| e1=located(exprOrOp) p=located(ISGT) e2=located(exprPlusMinusOps)
{
  mkbinop e1 p e2 ">?"
}

(* exprAssign sert à gérer le cas de l'assignation d'une valeur à une référence *)
exprAssign:
| e=exprCompBinOp
{
  e
}
| e1=located(exprApplyFun) ASSIGN e2=located(exprAssign)
{
  Assign (e1, e2)
}

(* exprAnonymFun sert à gérer le cas des fonctions anonymes *)
exprAnonymFun:
| e=exprAssign
{
  e
}
| BACKSLASH p=located(pattern) ARROW e=located(exprAnonymFun)
{
  Fun (FunctionDefinition (p, e))
}

(* expr sert à gérer le cas de la séparation des séquences d'expressions 
ou d'une définition locale suivit d'expressions *)
expr:
| e=exprAnonymFun
{
  e
}
| e1=located(exprAnonymFun) SEMICOLON e2=located(expr)
{
  Sequence [e1; e2]
}
| v=vdefinition(exprAnonymFun) SEMICOLON e=located(expr)
{
  Define (v,e)
}

branch:
| p=located(pattern) ARROW e=located(expr)
{
  Branch (p, e)
}

(* record_elem_pattern sert à parser les briques élémentaires de pattern d'un record *)
record_elem_pattern:
| l=located(label) EQUAL p=located(pattern)
{
  (l, p)
}

(* patternNonAmbiguous gère tout les cas de pattern ayant des structures non ambigues *)
patternNonAmbiguous:
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

(* pattern gère les cas de pattern ambigues *)
pattern:
| p=located(patternNonAmbiguous) COLON t=located(tyTuples)
{
  PTypeAnnotation (p, t)
}
| p1=located(patternNonAmbiguous) BAR p2=separated_nonempty_list(BAR, located(patternNonAmbiguous))
{
  POr (p1 :: p2)
}
| p1=located(patternNonAmbiguous) AMPERSAND p2=separated_nonempty_list(AMPERSAND, located(patternNonAmbiguous))
{
  PAnd (p1 :: p2)
}
| p=patternNonAmbiguous
{
  p
}


%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
