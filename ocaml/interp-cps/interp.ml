(* Type definitions *)

open Luaparser.Ast

type value = Value.t

type coroutine = Value.coroutine

type env = Value.env

(* Some useful functions *)

let create_scope (names : string list) (values : value list) :
  (name, value) Hashtbl.t =
  let htbl = Hashtbl.create 16 in

  let rec loop l1 l2 =
    match (l1, l2) with
    | [], _ -> ()
    | name :: l1', [] ->
      Hashtbl.replace htbl name Value.Nil;
      loop l1' l2
    | name :: l1', value :: l2' ->
      Hashtbl.replace htbl name value;
      loop l1' l2'
  in

  loop names values;
  htbl

(* Interpretation functions *)

let rec interp_block (env : env) (co : coroutine) (blk : block)
  (k : value -> unit) : unit =
  (* le padding avec Value.Nil est geré par create_scope *)
  let local_scope = create_scope blk.locals [] in
  let env = { env with locals = local_scope :: env.locals } in

  interp_stat env co blk.body (fun () -> interp_exp env co blk.ret k)

and interp_stat (env : env) (co : coroutine) (stat : stat) (k : unit -> unit) :
  unit =
  match stat with
  | Nop -> k ()
  | Seq (s1, s2) -> interp_stat env co s1 (fun () -> interp_stat env co s2 k)
  | Assign (Name name, exp) ->
    interp_exp env co exp @@ fun value ->
    Value.set_ident env name value;
    k ()
  | Assign (IndexTable (table, key), exp) ->
    interp_exp env co table @@ fun table_v ->
    interp_exp env co key @@ fun key_v ->
    interp_exp env co exp @@ fun value ->
    let table = Value.as_table table_v in
    let key = Value.as_table_key key_v in
    Hashtbl.replace table key value;
    k ()
  | FunctionCall f -> interp_funcall env co f (fun _ -> k ())
  | WhileDoEnd (exp, stat) -> while_do_end env co exp stat k
  | If (exp, s1, s2) ->
    interp_exp env co exp @@ fun value ->
    if Value.as_bool value then interp_stat env co s1 k
    else interp_stat env co s2 k

and while_do_end env co exp stat k =
  interp_exp env co exp @@ fun value ->
  if Value.as_bool value then
    interp_stat env co stat (fun () -> while_do_end env co exp stat k)
  else k ()

and interp_funcall (env : env) (co : coroutine) (fc : functioncall)
  (k : value -> unit) : unit =
  let f, args = fc in
  interp_exp env co f @@ fun func ->
  match Value.as_function func with
  | Closure (params, local_env, block) ->
    let env = create_fun_env env local_env co args params in
    interp_block env co block k
  | Print ->
    print env co args;
    k Value.Nil
  | CoroutCreate -> begin
    let f = eval_args env co args |> List.hd |> Value.as_function in
    match f with
    | Closure (params, local_env, block) ->
      let corout = Value.{ stat = Dead } in

      let corout_k arg =
        let local_scope = create_scope params [ arg ] in
        let env = { local_env with locals = local_scope :: local_env.locals } in

        interp_block env corout block @@ fun value ->
        match corout.stat with
        | Dead -> failwith "The current coroutine is already dead"
        | Running k' ->
          corout.stat <- Dead;
          k' value
        | Suspended _ -> assert false
      in

      corout.stat <- Suspended corout_k;
      k @@ Coroutine corout
    | _ -> failwith "The argument given to coroutine.create is not a function"
  end
  | CoroutResume -> begin
    let corout, arg =
      match eval_args env co args with
      | [ corout ] -> (Value.as_coroutine corout, Value.Nil)
      | [ corout; arg ] -> (Value.as_coroutine corout, arg)
      | _ -> failwith "There is too many arguments in coroutine.mini_resume"
    in

    match corout.stat with
    | Dead -> failwith "The coroutine is dead"
    | Running _ -> ()
    | Suspended k' ->
      corout.stat <- Running k;
      k' arg
  end
  | CoroutYield -> begin
    let arg =
      match eval_args env co args with
      | [] -> Value.Nil
      | [ arg ] -> arg
      | _ -> failwith "There is too many arguments in coroutine.yield"
    in

    match co.stat with
    | Dead -> failwith "The coroutine is dead"
    | Running k' ->
      co.stat <- Suspended k;
      k' arg
    | Suspended _ -> ()
  end
  | CoroutStatus -> begin
    let corout = eval_args env co args |> List.hd |> Value.as_coroutine in

    let str =
      match corout.stat with
      | Dead -> "dead"
      | Running _ -> "running"
      | Suspended _ -> "suspended"
    in
    Value.String str |> k
  end

and create_fun_env (env : env) (local_env : env) (co : coroutine) (args : args)
  (params : name list) : env =
  (* la possible différence de longueur entre les
     paramètres et les arguments est geré par create_scope *)
  let args_evaluated = eval_args env co args in
  let local_scope = create_scope params args_evaluated in
  { local_env with locals = local_scope :: local_env.locals }

and eval_args (env : env) (co : coroutine) (args : args) : value list =
  let res = ref [] in
  let rec loop l acc =
    match l with
    | [] -> res := List.rev acc
    | exp :: l' -> interp_exp env co exp @@ fun value -> loop l' (value :: acc)
  in
  loop args [];
  !res

and print (env : env) (co : coroutine) (args : args) : unit =
  match args with
  | [] -> ()
  | [ exp ] ->
    interp_exp env co exp (fun value ->
        Printf.printf "%s\n" (value |> Value.to_string) )
  | exp :: args' ->
    interp_exp env co exp (fun value ->
        Printf.printf "%s\t" (value |> Value.to_string) );
    print env co args'

and interp_exp (env : env) (co : coroutine) (e : exp) (k : value -> unit) : unit
    =
  match e with
  (* expressions avec k interne *)
  | Var (IndexTable (table, key)) ->
    interp_exp env co table @@ fun value_tbl ->
    interp_exp env co key @@ fun value_k ->
    let table = Value.as_table value_tbl in
    let key = Value.as_table_key value_k in
    let value = Hashtbl.find_opt table key |> Option.value ~default:Value.Nil in
    k value
  | FunctionCallE f -> interp_funcall env co f k
  | BinOp (op, e1, e2) -> begin
    interp_exp env co e1 @@ fun v1 ->
    match op with
    | LogicalAnd -> if Value.as_bool v1 then interp_exp env co e2 k else k v1
    | LogicalOr -> if Value.as_bool v1 then k v1 else interp_exp env co e2 k
    | _ ->
      interp_exp env co e2 @@ fun v2 ->
      let value =
        match op with
        (* arithmetic operators *)
        | Addition -> Value.add v1 v2
        | Subtraction -> Value.sub v1 v2
        | Multiplication -> Value.mul v1 v2
        (* relational operators *)
        | Equality -> Value.Bool (Value.equal v1 v2)
        | Inequality -> Value.Bool (Value.equal v1 v2 |> not)
        | Less -> Value.Bool (Value.lt v1 v2)
        | Greater -> Value.Bool (Value.le v1 v2 |> not)
        | LessEq -> Value.Bool (Value.le v1 v2)
        | GreaterEq -> Value.Bool (Value.lt v1 v2 |> not)
        (* logical operators *)
        | _ -> assert false
      in
      k value
  end
  | UnOp (op, exp) -> begin
    interp_exp env co exp @@ fun value ->
    let value' =
      match op with
      | UnaryMinus -> Value.neg value
      | Not -> Value.Bool (not @@ Value.as_bool value)
    in
    k value'
  end
  (* expressions avec k externe *)
  | _ -> begin
    let value =
      match e with
      | Nil -> Value.Nil
      | False -> Value.Bool false
      | True -> Value.Bool true
      | Integer n -> Value.Int n
      | Float f -> Value.Float f
      | LiteralString s -> Value.String s
      | Var (Name name) -> Value.lookup_ident env name
      | FunctionDef fun_body ->
        let params, code = fun_body in
        let f = Value.Closure (params, env, code) in
        Value.Function f
      | Table items ->
        let htbl = Hashtbl.create 16 in
        add_items env co htbl items;
        Value.Table htbl
      | _ -> assert false
    in
    k value
  end

and add_items (env : env) (co : coroutine)
  (table : (Value.tkey, value) Hashtbl.t) (items : (exp * exp) list) : unit =
  match items with
  | [] -> ()
  | (k, v) :: items' ->
    interp_exp env co k @@ fun key ->
    interp_exp env co v @@ fun value ->
    let key = Value.as_table_key key in
    Hashtbl.replace table key value;
    add_items env co table items'

let run ast =
  let coroutine : (Value.tkey, value) Hashtbl.t = Hashtbl.create 4 in
  Hashtbl.add coroutine (KString "create") (Value.Function CoroutCreate);
  Hashtbl.add coroutine (KString "yield") (Value.Function CoroutYield);
  Hashtbl.add coroutine (KString "mini_resume") (Value.Function CoroutResume);
  Hashtbl.add coroutine (KString "status") (Value.Function CoroutStatus);

  let globals : (string, value) Hashtbl.t = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Function Print);
  Hashtbl.add globals "coroutine" (Table coroutine);

  let env = Value.{ globals; locals = [] } in
  let co = Value.{ stat = Running (fun _ -> ()) } in

  interp_block env co ast (fun _ -> ())
