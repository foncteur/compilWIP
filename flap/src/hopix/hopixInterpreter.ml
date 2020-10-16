open Position
open Error
open HopixAST

(** [error pos msg] reports execution error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of Hopix evaluates into a [value].

   The [value] type is not defined here. Instead, it will be defined
   by instantiation of following ['e gvalue] with ['e = environment].
   Why? The value type and the environment type are mutually recursive
   and since we do not want to define them simultaneously, this
   parameterization is a way to describe how the value type will use
   the environment type without an actual definition of this type.

*)
type 'e gvalue =
  | VInt       of Mint.t
  | VChar      of char
  | VString    of string
  | VUnit
  | VTagged    of constructor * 'e gvalue list
  | VTuple     of 'e gvalue list
  | VRecord    of (label * 'e gvalue) list
  | VLocation  of Memory.location
  | VClosure   of 'e * pattern located * expression located
  | VPrimitive of string * ('e gvalue Memory.t -> 'e gvalue -> 'e gvalue)

(** Two values for booleans. *)
let ptrue  = VTagged (KId "True", [])
let pfalse = VTagged (KId "False", [])

(**
    We often need to check that a value has a specific shape.
    To that end, we introduce the following coercions. A
    coercion of type [('a, 'e)] coercion tries to convert an
    Hopix value into a OCaml value of type ['a]. If this conversion
    fails, it returns [None].
*)

type ('a, 'e) coercion = 'e gvalue -> 'a option
let fail = None
let ret x = Some x
let value_as_int      = function VInt x -> ret x | _ -> fail
let value_as_char     = function VChar c -> ret c | _ -> fail
let value_as_string   = function VString s -> ret s | _ -> fail
let value_as_tagged   = function VTagged (k, vs) -> ret (k, vs) | _ -> fail
let value_as_record   = function VRecord fs -> ret fs | _ -> fail
let value_as_location = function VLocation l -> ret l | _ -> fail
let value_as_closure  = function VClosure (e, p, b) -> ret (e, p, b) | _ -> fail
let value_as_primitive = function VPrimitive (p, f) -> ret (p, f) | _ -> fail
let value_as_bool = function
  | VTagged (KId "True", []) -> true
  | VTagged (KId "False", []) -> false
  | _ -> assert false

(**
   It is also very common to have to inject an OCaml value into
   the types of Hopix values. That is the purpose of a wrapper.
 *)
type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x
let bool_as_value b = if b then ptrue else pfalse

(**

  The flap toplevel needs to print the result of evaluations. This is
   especially useful for debugging and testing purpose. Do not modify
   the code of this function since it is used by the testsuite.

*)
let print_value m v =
  (** To avoid to print large (or infinite) values, we stop at depth 5. *)
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
          Mint.to_string x
        | VChar c ->
          "'" ^ Char.escaped c ^ "'"
        | VString s ->
          "\"" ^ String.escaped s ^ "\""
        | VUnit ->
          "()"
        | VLocation a ->
          print_array_value d (Memory.dereference m a)
        | VTagged (KId k, []) ->
          k
        | VTagged (KId k, vs) ->
          k ^ print_tuple d vs
        | VTuple (vs) ->
           print_tuple d vs
        | VRecord fs ->
           "{"
           ^ String.concat ", " (
                 List.map (fun (LId f, v) -> f ^ " = " ^ print_value (d + 1) v
           ) fs) ^ "}"
        | VClosure _ ->
          "<fun>"
        | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
    and print_tuple d vs =
      "(" ^ String.concat ", " (List.map (print_value (d + 1)) vs) ^ ")"
    and print_array_value d block =
      let r = Memory.read block in
      let n = Mint.to_int (Memory.size block) in
      "[ " ^ String.concat ", " (
                 List.(map (fun i -> print_value (d + 1) (r (Mint.of_int i)))
                         (ExtStd.List.range 0 (n - 1))
               )) ^ " ]"
  in
  print_value 0 v

let print_values m vs =
  String.concat "; " (List.map (print_value m) vs)

module Environment : sig
  (** Evaluation environments map identifiers to values. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)
  val bind    : t -> identifier -> t gvalue -> t

  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)
  val update  : Position.t -> identifier -> t -> t gvalue -> unit

  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)
  val lookup  : Position.t -> identifier -> t -> t gvalue

  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)
  exception UnboundIdentifier of identifier * Position.t

  (** [last env] returns the latest binding in [env] if it exists. *)
  val last    : t -> (identifier * t gvalue * t) option

  (** [print env] returns a human readable representation of [env]. *)
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v =
    lookup' pos x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding m (Id x, v) =
    x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

(**
    We have everything we need now to define [value] as an instantiation
    of ['e gvalue] with ['e = Environment.t], as promised.
*)
type value = Environment.t gvalue

(**
   The following higher-order function lifts a function [f] of type
   ['a -> 'b] as a [name]d Hopix primitive function, that is, an
   OCaml function of type [value -> value].
*)
let primitive name ?(error = fun () -> assert false) coercion wrapper f
: value
= VPrimitive (name, fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

type runtime = {
  memory      : value Memory.t;
  environment : Environment.t;
}

type observable = {
  new_memory      : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    let error m v =
      Printf.eprintf
        "Invalid arguments for `%s': %s\n"
        name (print_value m v);
      assert false (* By typing. *)
    in
    VPrimitive (name, fun m -> function
      | VInt x ->
         VPrimitive (name, fun m -> function
         | VInt y -> out (op x y)
         | v -> error m v)
      | v -> error m v)
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v))
      x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name =
    intbin name (fun x -> VInt x) in
  let binarithops = Mint.(
    [ ("`+`", add); ("`-`", sub); ("`*`", mul); ("`/`", div) ]
  ) in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name bool_as_value in
  let cmparithops =
    [ ("`=?`", ( = ));
      ("`<?`", ( < ));
      ("`>?`", ( > ));
      ("`>=?`", ( >= ));
      ("`<=?`", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, fun _ x -> VPrimitive (name, fun _ y ->
        out (op (value_as_bool x) (value_as_bool y))))
  in
  let boolarith name = boolbin name (fun x -> if x then ptrue else pfalse) in
  let boolarithops =
    [ ("`||`", ( || )); ("`&&`", ( && )) ]
  in
  let generic_printer =
    VPrimitive ("print", fun m v ->
      output_string stdout (print_value m v);
      flush stdout;
      VUnit
    )
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive  ("print_int", fun _ -> function
      | VInt x -> print (Mint.to_string x)
      | _ -> assert false (* By typing. *)
    )
  in
  let print_string =
    VPrimitive  ("print_string", fun _ -> function
      | VString x -> print x
      | _ -> assert false (* By typing. *)
    )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print"        generic_printer
  |> bind' "print_int"    print_int
  |> bind' "print_string" print_string
  |> bind' "true"         ptrue
  |> bind' "false"        pfalse
  |> bind' "nothing"      VUnit

let initial_runtime () = {
  memory      = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
  environment = primitives;
}

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(** [definition pos runtime d] evaluates the new definition [d]
    into a new runtime [runtime']. In the specification, this
    is the judgment:

                        E, M ⊢ dv ⇒ E', M'

*)
and def_funList environment = function 
  | [] -> environment
  | (name, _, HopixAST.FunctionDefinition (m, e))::fun_list ->
    let fun_def = VClosure (environment, m, e) in 
    let new_env = Environment.bind environment (value name) fun_def in
    Environment.update (position name) (value name) new_env (VClosure (new_env, m, e));
    def_funList new_env fun_list

and funList_update_env environment = function 
  | [] -> ()
  | (name, _, HopixAST.FunctionDefinition (m, e))::fun_list ->
    Environment.update (position name) (value name) environment (VClosure (environment, m, e));
    funList_update_env environment fun_list

and definition runtime d = match (value d) with
  | HopixAST.DefineType _      -> runtime
  | HopixAST.DeclareExtern _   -> runtime

  | HopixAST.DefineValue (HopixAST.SimpleValue (name, _, e)) ->
    let eval = expression' runtime.environment runtime.memory e in
    let new_env = Environment.bind runtime.environment (value name) eval in
    {environment = new_env ; memory = runtime.memory}

  | HopixAST.DefineValue (HopixAST.RecFunctions fun_list) ->
    let new_env = def_funList runtime.environment fun_list in 
    funList_update_env new_env fun_list;
    { environment = new_env ; memory = runtime.memory }
    
(* This is a function that interprets a literal *)
and interpret_literal = function
| LInt i -> int_as_value i
| LChar c -> VChar c 
| LString s -> VString s

and expression' environment memory e =
  expression (position e) environment memory (value e)

(** [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)

and expression pos environment memory = function
  | HopixAST.Literal i ->
      interpret_literal (value i)

  | HopixAST.Variable (id, _) ->
      let eval = Environment.lookup (position id) (value id) environment in
      eval

  | HopixAST.Tagged (k, _, exs) ->
      let eval = List.map (expression' environment memory) exs in
      VTagged (value k, eval)

  | HopixAST.Tuple exs ->
      let eval = List.map (expression' environment memory) exs in
      VTuple eval

  | HopixAST.Record (recs, _) ->
      let eval_record = function (l, e) -> (value l, expression' environment memory e) in 
      let eval_recs = List.map eval_record recs in
      VRecord eval_recs

  | HopixAST.Field (e, l) ->
      let eval_e = expression' environment memory e in
      (
        match (value_as_record eval_e) with 
         | Some r -> List.find (function (label, _) -> label = (value l)) r |> snd
         | None -> error [pos] "Field - eval_e is not a VRecord"
      )

  | HopixAST.Sequence exs ->
      let rec eval_seq = function
        | []    -> error [pos] "Sequence - empty list"
        | [e]   -> expression' environment memory e 
        | e::l  -> let _ = expression' environment memory e in 
            eval_seq l 
      in 
      eval_seq exs

  | HopixAST.Define (vd, e) ->
      let actual_runtime = { memory = memory ; environment = environment } in 
      let new_runtime = definition actual_runtime (Position.with_pos pos (HopixAST.DefineValue vd)) in 
      expression' new_runtime.environment new_runtime.memory e

  | HopixAST.Fun (FunctionDefinition (p, f)) -> 
      VClosure (environment, p, f)

  | HopixAST.Apply (efun, earg) ->
      let eval_fun = expression' environment memory efun in
      let eval_arg = expression' environment memory earg in
      (
        match eval_fun with
          | VPrimitive (_, f) -> f memory eval_arg
          | VClosure (env_f, p, f) -> 
              let (patternOk, new_env) = pattern env_f eval_arg (value p) in 
              if patternOk then 
                expression' new_env memory f 
              else 
                error [pos] "Apply - eval2 doesn't match with eval1 pattern"

          | _ -> error [pos] "Apply - eval1 doesn't recognized"
      )

  | HopixAST.Ref e ->
      let eval = expression' environment memory e in
      let loc = Memory.allocate memory (Mint.of_int 1) eval in
      VLocation loc

  | HopixAST.Read e ->
      let eval = expression' environment memory e in
      (
        match (value_as_location eval) with 
          | Some loc -> let block = Memory.dereference memory loc in 
              Memory.read block (Mint.of_int 0)
          | None -> error [pos] "Read - eval is not a VLocation"
      )

  | HopixAST.Assign (eref, eval) ->
      let evalref = expression' environment memory eref in 
      (
        match (value_as_location evalref) with 
          | Some loc -> let block = Memory.dereference memory loc in 
              let evalval = expression' environment memory eval in 
              Memory.write block (Mint.of_int 0) evalval;
              VUnit
          | None -> error [pos] "Assign - evalref is not a VLocation"
      )

  | HopixAST.Case (e, b) ->
      let eval = expression' environment memory e in 
      let rec findPattern = function 
        | [] -> error [pos] "Case - no pattern matches"
        | b_loc::bs -> let b = value b_loc in 
            ( 
              match b with Branch (p, exprToExec) ->
                let (patternFound, new_env) = pattern environment eval (value p) in  
                if patternFound then 
                  expression' new_env memory exprToExec
                else 
                  findPattern bs
            )
      in findPattern b 


  | HopixAST.IfThenElse (econd, etrue, efalse) ->
      let evalcond = expression' environment memory econd in
      (
        try (
          if (value_as_bool evalcond) then 
            expression' environment memory etrue
          else 
            expression' environment memory efalse
        ) with 
          | _ -> error [pos] "IfThenElse - evalcond is not a bool"
      )
      
  | HopixAST.While (econd, ebody) ->
      let evalcond = ref (expression' environment memory econd) in
      (
        try (
          while (value_as_bool !evalcond) do 
            let _ = expression' environment memory ebody in
            evalcond := expression' environment memory econd
          done;
          VUnit
        ) with 
            | _ -> error [pos] "While - evalcond is not a bool"
      )

  | HopixAST.For (id, estart, estop, ebody) ->
      let evalstart = expression' environment memory estart in 
      let evalstop = expression' environment memory estop in 
      (
        match (value_as_int evalstart, value_as_int evalstop) with 
            | (Some nstart, Some nstop) ->
                for x = (Mint.to_int nstart) to (Mint.to_int nstop) do 
                  let new_env = Environment.bind environment (value id) (VInt (Mint.of_int x)) in 
                  let _ = expression' new_env memory ebody in 
                  ()
                done;
                VUnit
            | _ -> error [pos] "For - evalstart or evalstop is not a int"
      )

  | HopixAST.TypeAnnotation (e, _) -> expression' environment memory e 

and analyse_patterns_tagged environment vs ps =
    match (vs, ps) with 
      | ([], []) -> (true, environment)
      | (v::vs, p::ps) -> 
          let (patternOk, new_env) = pattern environment v (value p) in
          (
            if patternOk then
              analyse_patterns_tagged new_env vs ps 
            else 
              (false, environment)
          )
      | _ -> error [] "analyse_patterns_tagged"

and analyse_patterns_rec environment vrec prec = 
    match (vrec, prec) with 
      | ([], []) -> (true, environment)
      | ( (vl, vval)::vrec, (pl, p)::prec ) ->
          if vl = value pl then (
            let (patternOk, new_env) = pattern environment vval (value p) in 
            if patternOk then 
              analyse_patterns_rec new_env vrec prec
            else 
              (false, environment)
          ) else 
            (false, environment)
      | _ -> error [] "analyse_patterns_rec"

and analyse_patterns_and environment v = function 
    | [] -> (true, environment)
    | p::ps -> let (patternOk, new_env) = pattern environment v (value p) in 
          if patternOk then 
            analyse_patterns_and new_env v ps
          else 
            (false, environment)

and analyse_patterns_or environment v = function
    | [] -> (false, environment)
    | p::ps -> let (patternOk, new_env) = pattern environment v (value p) in 
          if patternOk then 
            (true, new_env)
          else 
            analyse_patterns_or environment v ps

and pattern environment v = function
  | PVariable id ->
      let new_env = Environment.bind environment (value id) v in
      (true, new_env)

  | PWildcard -> (true, environment)

  | PTypeAnnotation (p, _) -> pattern environment v (value p)

  | PLiteral l ->
      ( 
        match (value l) with 
          | LInt i -> (Some i = value_as_int v, environment)
          | LChar c -> (Some c = value_as_char v, environment)
          | LString s -> (Some s = value_as_string v, environment)
      )

  | PTaggedValue (k, _, ps) ->
      (
        match (value_as_tagged v) with
          | Some (k_v, exs_v) ->
              if k_v = (value k) then 
                analyse_patterns_tagged environment exs_v ps
              else 
                (false, environment)
          | None -> (false, environment)
      )

  | PRecord (prec, _) ->
      (
        match (value_as_record v) with 
          | Some vrec -> analyse_patterns_rec environment vrec prec
          | None -> (false, environment)
      )

  | PTuple ps -> 
      (
        match v with 
          | VTuple vs -> analyse_patterns_tagged environment vs ps
          | _ -> (false, environment)
      )

  | POr ps -> analyse_patterns_or environment v ps

  | PAnd ps -> analyse_patterns_and environment v ps
  

(** This function returns the difference between two runtimes. *)
and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory =
      runtime'.memory
  }

(** This function displays a difference between two runtimes. *)
let print_observable (_ : runtime) observation =
  Environment.print observation.new_memory observation.new_environment
