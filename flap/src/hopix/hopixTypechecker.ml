(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  (**
      We check the presence of type ascriptions on:
      - variables
      - tagged values patterns
   *)
  let rec program p = List.iter (located definition) p

  and definition _ = function
    | DefineValue vdef ->
      value_definition vdef
    | _ ->
      ()

  and value_definition = function
    (** A toplevel definition for a value. *)
    | SimpleValue (x, s, e) ->
       if s = None then missing_type_annotation (Position.position x);
       located expression e
    (** A toplevel definition for mutually recursive functions. *)
    | RecFunctions fs ->
       List.iter function_definition fs

  and function_definition = function
    | (f, s, FunctionDefinition (_, e)) ->
       if s = None then missing_type_annotation (Position.position f);
       located expression e

  and expression pos = function
    | Define (vdef, e) ->
       value_definition vdef;
       located expression e
    | Apply (a, b) ->
       List.iter (located expression) [a; b]
    | Tuple ts ->
       List.iter (located expression) ts
    | Record (fields, a) ->
       if a = None then type_error pos "A type annotation is missing.";
       List.iter (fun (_, e) -> located expression e) fields
    | TypeAnnotation ({ Position.value = Fun (FunctionDefinition (_, e)) },
                      _) ->
       located expression e
    | Fun (FunctionDefinition (_, _)) ->
       type_error pos "An anonymous function must be annotated."
    | Field (e, _) | TypeAnnotation (e, _) | Ref e | Read e ->
       located expression e
    | Sequence es ->
       List.iter (located expression) es
    | Tagged (_, a, es) ->
       if a = None then type_error pos "A type annotation is missing.";
       List.iter (located expression) es
    | For (_, e1, e2, e3) ->
       List.iter (located expression) (
           [ e1; e2; e3 ]
         )
    | IfThenElse (c, t, f) ->
       List.iter (located expression) [c; t; f]
    | Case (e, bs) ->
      located expression e;
      List.iter (located branch) bs
    | Assign (e1, e2) | While (e1, e2) ->
      located expression e1;
      located expression e2
    | Literal _ | Variable _ ->
      ()
  and pattern pos = function
    | PTypeAnnotation ({ Position.value = (PWildcard | PVariable _) }, _) ->
      ()
    | PRecord (fields, a) ->
       if a = None then type_error pos "A type annotation is missing.";
       List.iter (fun (_, p) -> located pattern p) fields
    | PTuple ps ->
       List.iter (located pattern) ps
    | PTypeAnnotation (p, _) ->
      located pattern p
    | PVariable _ | PWildcard ->
      missing_type_annotation pos
    | PTaggedValue (_, a, ps) ->
       if a = None then type_error pos "A type annotation is missing.";
       List.iter (located pattern) ps
    | POr ps | PAnd ps ->
      List.iter (located pattern) ps
    | PLiteral _ ->
      ()
  and branch _ = function
    | Branch (p, e) ->
      located pattern p;
      located expression e
  and missing_type_annotation pos =
    type_error pos "A type annotation is missing."
  in
  program ast

let invalid_instantiation pos given expected =
  type_error pos (
      Printf.sprintf
        "Invalid number of types in instantiation: \
         %d given while %d were expected." given expected
    )

let safe_instantiate_type_scheme pos scheme types =
  try
    instantiate_type_scheme scheme types
  with
  | InvalidInstantiation (expected, given) ->
    invalid_instantiation pos given expected

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast : typing_environment =
  check_program_is_fully_annotated ast;

  let rec program p =
    List.fold_left (fun env x -> located (definition env) x) tenv p

  and definition tenv _ = function
    | DefineValue vdef ->
       value_definition tenv vdef

    | DefineType (t, ts, tdef) ->
       let ts = List.map Position.value ts in
       HopixTypes.bind_type_definition (Position.value t) ts tenv tdef

    | DeclareExtern (x, s) ->
       let s = located (type_scheme tenv) s in
       bind_value (Position.value x) s tenv

  and type_scheme tenv pos (ForallTy (ts, ty)) =
    let ts = List.map Position.value ts in
    let tenv = bind_type_variables pos tenv ts in
    Scheme (ts, internalize_ty tenv ty)

  and bind_type_variables pos tenv ts =
    List.iter (fun v ->
        if HopixTypes.is_type_variable_defined pos tenv v then
          type_error pos (
              Printf.sprintf
                "The type variable `%s' is already bound in the environment."
                (HopixPrettyPrinter.(to_string type_variable v))
            )
      ) ts;
    HopixTypes.bind_type_variables pos tenv ts

  and value_definition (tenv : typing_environment) = function
    | SimpleValue (x, Some s, e) ->
       let pos = Position.position s in
       let Scheme (ts, aty) as s = located (type_scheme tenv) s in
       let tenv' = bind_type_variables pos tenv ts in
       check_expression_monotype tenv' aty e;
       bind_value (Position.value x) s tenv

    | SimpleValue (_, _, _) ->
       assert false (* By check_program_is_fully_annotated. *)

    | RecFunctions fs ->
       recursive_definitions tenv fs

  and recursive_definitions tenv recdefs =
    let tenv =
      List.fold_left (fun tenv (f, fs, _) ->
          match fs with
          | None ->
             assert false  (* By check_program_is_fully_annotated. *)
          | Some fs ->
             let f = Position.value f in
             let fs = located (type_scheme tenv) fs in
             let fs = refresh_type_scheme fs in
             bind_value f fs tenv
        ) tenv recdefs
    in
    List.iter (fun (f, fs, d) ->
        match fs with
        | None ->
           assert false
        | Some fs ->
           let pos = Position.position f in
           let fs = located (type_scheme tenv) fs in
           check_function_definition pos tenv fs d
      ) recdefs;
    tenv

  (** [check_function_definition tenv fdef] checks that the
      function definition [fdef] is well-typed with respect to the
      type annotations written by the programmer. We assume that
      [tenv] already contains the type scheme of the function [f]
      defined by [fdef] as well as all the functions which are
      mutually recursively defined with [f]. *)
  and check_function_definition pos tenv aty = function
    | FunctionDefinition (p, e) ->
       match aty with
       | Scheme (ts, ATyArrow (_, out)) ->
          let tenv = bind_type_variables pos tenv ts in
          let tenv, _ , _= located (pattern tenv) p in
          check_expression_monotype tenv out e
       | _ ->
          type_error pos "A function must have an arrow type."

  (** [check_expected_type pos xty ity] verifies that the expected
      type [xty] is syntactically equal to the inferred type [ity]
      and raises an error otherwise. *)
  and check_expected_type pos xty ity =
    if xty <> ity then
      type_error pos (
          Printf.sprintf "Type error:\nExpected:\n  %s\nGiven:\n  %s\n"
            (print_aty xty) (print_aty ity)
        )

  (** [check_expression_monotype tenv xty e] checks if [e] has
      the monotype [xty] under the context [tenv]. *)
  and check_expression_monotype tenv xty e : unit =
    let pos = Position.position e in
    let ity = located (type_of_expression tenv) e in
    check_expected_type pos xty ity

  and type_of_literal : literal -> aty = function
    | LInt _    -> hint
    | LString _ -> hstring
    | LChar _   -> hchar

  and id_of_id_loc id_loc =
    match (Position.value id_loc) with
      | Id id   -> id

  and type_of_types ty_loc =
    match (Position.value ty_loc) with
      | TyCon (ty_con, tys) -> ATyCon (ty_con, List.map type_of_types tys)
      | TyArrow (ty1, ty2) -> ATyArrow (type_of_types ty1, type_of_types ty2)
      | TyTuple tys -> ATyTuple (List.map type_of_types tys)
      | TyVar ty_var -> ATyVar ty_var

  and apply_function tenv pos fty e =
    let (inty, outty) = match fty with
    | ATyArrow (inty, outty) -> (inty, outty)
    | _ -> type_error pos "Only functions can be applied.\n"
    in
    check_expression_monotype tenv inty e;
    outty

  (** [type_of_expression tenv pos e] computes a type for [e] if it exists. *)
  and type_of_expression tenv pos : expression -> aty = function
    | Literal l ->
        type_of_literal (Position.value l)

    | Variable (id_loc, tys) ->
        let tys = match tys with None -> [] | Some tys -> tys in
        let types = List.map aty_of_ty' tys in
        let scheme_var = try
          located lookup_type_scheme_of_value id_loc tenv
        with
        | UnboundIdentifier (pos, Id x) ->
          type_error pos (Printf.sprintf "Unbound value `%s'.\n" x)
        in
        safe_instantiate_type_scheme pos scheme_var types

    | Tagged (con_loc, None, exs) ->
        assert false (* by check_program_is_fully_annotated *)

    | Tagged (con_loc, Some tys, exs) ->
        let types = List.map aty_of_ty' tys in
        let scheme_con = try
          lookup_type_scheme_of_constructor (Position.value con_loc) tenv
        with
        | UnboundConstructor ->
          let KId x = Position.value con_loc in
          type_error pos (Printf.sprintf "Unbound constructor `%s'.\n" x)
        in
        let cty = safe_instantiate_type_scheme pos scheme_con types in
        (* let ins, out = destruct_arrows cty in
        if List.length ins <> List.length exs then
          type_error pos (Printf.sprintf "Incorrect number of arguments to constructor.\n");
        List.iter2 (check_expression_monotype tenv) ins exs;
        out *)
        List.fold_left (apply_function tenv pos) cty exs

    | Record (fields, None) ->
        assert false (* by check_program_is_fully_annotated *)

    | Record (fields, Some tys) ->
        let types = List.map aty_of_ty' tys in
        assert (fields <> []);
        let f0 = Position.value (fst (List.hd fields)) in
        let tycon, arity, labels = try
          lookup_type_constructor_of_label f0 tenv
        with
        | UnboundLabel ->
          let LId x = f0 in
          type_error pos (Printf.sprintf "Label `%s' is unbound." x)
        in
        List.iter (fun (l, e) ->
          if not (List.mem (Position.value l) labels) then begin
            let LId x = Position.value l in
            let TCon y = tycon in
            type_error (Position.position l)
              (Printf.sprintf "Label `%s' does not belong to record `%s'" x y)
          end;
          let ts = lookup_type_scheme_of_label (Position.value l) tenv in
          let ety = match safe_instantiate_type_scheme pos ts types with
            | ATyArrow (_, out) -> out
            | _ -> assert false
          in
          let ty = located (type_of_expression tenv) e in
          if ety <> ty then begin
            let LId x = Position.value l in
            (* Note: typo in tests *)
            type_error pos (Printf.sprintf
              "The field `%s` as type `%s' while it should have type `%s'.\n"
              x (print_aty ty) (print_aty ety)
            )
          end
        ) fields;
        List.iter (fun l2 ->
          if not (List.exists (fun (l, e) -> l2 = Position.value l) fields) then
            let LId x = l2 in
            type_error pos (Printf.sprintf "Label `%s' is missing.\n" x)
        ) labels;
        ATyCon (tycon, types)

    | Field (e, l) ->
        let ty = located (type_of_expression tenv) e in
        let lty = try
            lookup_type_scheme_of_label (Position.value l) tenv
        with
        | UnboundLabel ->
          let LId x = Position.value l in
          type_error pos (Printf.sprintf "Label `%s' is unbound." x)
        in
        let (tycon, _, _) =
          lookup_type_constructor_of_label (Position.value l) tenv in
        (match ty with
        | ATyCon (tycon2, types) when tycon = tycon2 ->
          let rty = instantiate_type_scheme lty types in
          (match rty with
          | ATyArrow (ity, oty) -> assert (ity = ty); oty
          | _ -> assert false)
        | _ -> type_error pos "Incorrect type for record.\n")

    | Tuple es ->
        ATyTuple (List.map (located (type_of_expression tenv)) es)

    | Sequence [] ->
        hunit

    | Sequence es ->
        let rec iter = function
        | [] -> hunit
        | [e] -> located (type_of_expression tenv) e
        | e :: es ->
            check_expression_monotype tenv hunit e;
            iter es
        in iter es

    | Define (v, e) ->
        located (type_of_expression (value_definition tenv v)) e

    | TypeAnnotation
          ({ Position.value = Fun (FunctionDefinition (p, e)) }, ty) ->
        let aty = aty_of_ty' ty in
        let (aty1, aty2) = match aty with
        | ATyArrow (aty1, aty2) -> (aty1, aty2)
        | _ -> type_error pos "A function must have an arrow type."
        in
        let tenv, ty1, _ = located (pattern tenv) p in
        (* check_expected_type (Position.position p) ty1 aty1; *)
        check_expression_monotype tenv aty2 e;
        (* This is needed for correctness; putting it before the previous line
           produces better error messages (in the pattern instead of in the
           function) but is not compatible with the tests *)
        check_expected_type (Position.position p) ty1 aty1;
        aty

    | Fun _ ->
        assert false (* by check_program_is_fully_annotated *)
        (* Actually, we can easily handle this case. *)
        (*
          let tenv, ty1, _ = located (pattern tenv) p in
          let ty2 = located (type_of_expression tenv) e in
          ATyArrow (ty1, ty2)
        *)

    | Apply (e1, e2) ->
        let ty1 = located (type_of_expression tenv) e1 in
        apply_function tenv pos ty1 e2

    | Ref e ->
        href (located (type_of_expression tenv) e)

    | Assign (e1, e2) ->
        let ty = located (type_of_expression tenv) e1 in
        check_expression_monotype tenv (type_of_reference_type ty) e2;
        hunit

    | Read e ->
        let ty = located (type_of_expression tenv) e in
        type_of_reference_type ty

    | Case (e, bs) ->
        let ty = located (type_of_expression tenv) e in
        assert (bs <> []);
        let pattern_env b =
          let Branch (p, _) = Position.value b in
          let tenv, ty2, _ = located (pattern tenv) p in
          if ty <> ty2 then
            type_error (Position.position b)
              "This pattern is not compatible with the matched value.\n";
          tenv
        in
        let rty =
          let Branch (p, e) = Position.value (List.hd bs) in
          located (type_of_expression (pattern_env (List.hd bs))) e
        in
        List.iter (fun ({ Position.value = Branch (p, e) } as b) ->
          check_expression_monotype (pattern_env b) rty e) (List.tl bs);
        rty

    | IfThenElse (e1, e2, e3) ->
        check_expression_monotype tenv hbool e1;
        let ty = located (type_of_expression tenv) e2 in
        check_expression_monotype tenv ty e3;
        ty

    | While (e1, e2) ->
        check_expression_monotype tenv hbool e1;
        check_expression_monotype tenv hunit e2;
        hunit

    | For (x, e1, e2, e3) ->
        check_expression_monotype tenv hint e1;
        check_expression_monotype tenv hint e2;
        let nenv = bind_value (Position.value x) (monotype hint) tenv in
        check_expression_monotype nenv hunit e3;
        hunit

    | TypeAnnotation (e, ty) ->
        let aty = aty_of_ty' ty in
        check_expression_monotype tenv aty e;
        aty

  and patterns tenv = function
    | [] ->
       tenv, [], []
    | p :: ps ->
       let tenv, ty, def = located (pattern tenv) p in
       let tenv, tys, defs = patterns tenv ps in
       tenv, ty :: tys, check_disjoint_and_concat def defs

  and check_disjoint_and_concat vs1 vs2 =
    List.iter (fun v2 ->
      if List.exists (fun v1 -> Position.value v1 = Position.value v2) vs1 then
        type_error (Position.position v2)
          "This is the second occurrence of this variable in the pattern.\n"
    ) vs2;
    vs1 @ vs2

  (** [pattern tenv pos p] computes a new environment completed with
      the variables introduced by the pattern [p] as well as the type
      of this pattern, and the list of variables that were defined by
      the pattern (and their positions). *)
  and pattern tenv pos = function
    | PTypeAnnotation ({ Position.value = PWildcard }, ty) ->
      tenv, aty_of_ty' ty, []
    | PTypeAnnotation ({ Position.value = PVariable v }, ty) ->
      let aty = aty_of_ty' ty in
      bind_value (Position.value v) (monotype aty) tenv, aty, [v]
    | PRecord (fields, Some tys) ->
      let tys = List.map aty_of_ty' tys in
      assert (fields <> []);
      let f0 = Position.value (fst (List.hd fields)) in
      let tycon, arity, labels = try
        lookup_type_constructor_of_label f0 tenv
      with
      | UnboundLabel ->
        let LId x = f0 in
        type_error pos (Printf.sprintf
          "There is no type definition for the label `%s'." x)
      in
      let nenv, pts, def = patterns tenv (List.map snd fields) in
      List.iter2 (fun (l, p) ty ->
        if not (List.mem (Position.value l) labels) then begin
          let LId x = Position.value l in
          let TCon y = tycon in
          type_error (Position.position l)
            (Printf.sprintf "Label `%s' does not belong to record `%s'" x y)
        end;
        let ts = lookup_type_scheme_of_label (Position.value l) tenv in
        let ety = match safe_instantiate_type_scheme pos ts tys with
          | ATyArrow (_, out) -> out
          | _ -> assert false
        in
        if ety <> ty then begin
          let LId x = Position.value l in
          type_error pos (Printf.sprintf
            "The field `%s' has type `%s' while it should have type `%s'.\n"
            x (print_aty ty) (print_aty ety)
          )
        end
      ) fields pts;
      nenv, ATyCon (tycon, tys), def
    | PTuple ps ->
      let tenv, tys, def = patterns tenv ps in
      tenv, ATyTuple tys, def
    | PTypeAnnotation (p, ty) ->
      let aty = aty_of_ty' ty in
      let tenv, ty2, def = located (pattern tenv) p in
      check_expected_type pos aty ty2;
      tenv, aty, def
    | PVariable _ | PWildcard | PRecord (_, None) | PTaggedValue (_, None, _) ->
      assert false (* by check_program_is_fully_annotated *)
    | PTaggedValue (c, Some tys, ps) ->
      let types = List.map aty_of_ty' tys in
      let scheme = try
        lookup_type_scheme_of_constructor (Position.value c) tenv
      with
      | UnboundConstructor ->
        let KId x = Position.value c in
        type_error pos (Printf.sprintf "Unbound constructor `%s'.\n" x)
      in
      let cty = safe_instantiate_type_scheme pos scheme types in
      let ins, out = destruct_arrows cty in
      if List.length ins <> List.length ps then
        type_error pos "Invalid number of arguments to constructor.\n";
      let tenv, argtys, def = patterns tenv ps in
      List.iter2 (check_expected_type pos) ins argtys;
      tenv, out, def
    | POr ps ->
      let ls = List.map (located (pattern tenv)) ps in
      assert (ls <> []);
      let tenv1, ty1, def1 = List.hd ls in
      let dv = List.sort compare (List.map Position.value def1) in
      List.iter (fun (tenv, ty, def) ->
        if ty <> ty1 then
          type_error pos "All patterns must have the same type.\n";
        if List.sort compare (List.map Position.value def) <> dv then
          type_error pos "All patterns must bind the same variables.\n";
        List.iter (fun v ->
          let vty = located lookup_type_scheme_of_value v tenv in
          let vty1 = located lookup_type_scheme_of_value v tenv1 in
          if vty <> vty1 then
            type_error (Position.position v)
              "Variable must be bound to the same type in all branches.\n"
        ) def
      ) (List.tl ls);
      tenv1, ty1, def1
    | PAnd ps ->
      let tenv, tys, def = patterns tenv ps in
      assert (tys <> []);
      List.iter (fun t ->
        if t <> List.hd tys then
          type_error pos "All patterns must have the same type.\n")
        (List.tl tys);
      tenv, List.hd tys, def
    | PLiteral l ->
      tenv, type_of_literal (Position.value l), []
  in
  program ast


let print_typing_environment = HopixTypes.print_typing_environment
