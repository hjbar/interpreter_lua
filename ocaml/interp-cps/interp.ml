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
  | WhileDoEnd (exp, stat) ->
    let rec loop () =
      interp_exp env co exp @@ fun value ->
      if Value.as_bool value then interp_stat env co stat (fun () -> loop ())
      else k ()
    in
    loop ()
  | If (exp, s1, s2) ->
    interp_exp env co exp @@ fun value ->
    if Value.as_bool value then interp_stat env co s1 k
    else interp_stat env co s2 k

and interp_funcall (env : env) (co : coroutine) (fc : functioncall)
  (k : value -> unit) : unit =
  let eval_args env co args k =
    let rec loop args acc k =
      match args with
      | [] -> k @@ List.rev acc
      | exp :: args' ->
        interp_exp env co exp @@ fun value -> loop args' (value :: acc) k
    in
    loop args [] k
  in

  let f, args = fc in
  interp_exp env co f @@ fun func ->
  match Value.as_function func with
  | Closure (params, local_env, block) -> begin
    (* la possible différence de longueur entre les
       paramètres et les arguments est geré par create_scope *)
    eval_args env co args @@ fun args_evaluated ->
    let local_scope = create_scope params args_evaluated in
    let env = { local_env with locals = local_scope :: local_env.locals } in
    interp_block env co block k
  end
  | Print -> begin
    eval_args env co args @@ fun args_evaluated ->
    let () =
      List.map Value.to_string args_evaluated
      |> String.concat "\t" |> Format.printf "%s\n"
    in
    k Value.Nil
  end
  | CoroutCreate -> begin
    eval_args env co args @@ fun args_evaluated ->
    let f = List.hd args_evaluated |> Value.as_function in
    match f with
    | Closure (params, local_env, block) ->
      let corout = Value.{ stat = Dead } in

      let corout_k arg =
        (* la possible différence de longueur entre les
           paramètres et les arguments est geré par create_scope *)
        let local_scope = create_scope params [ arg ] in
        let env = { local_env with locals = local_scope :: local_env.locals } in

        interp_block env corout block @@ fun value ->
        match corout.stat with
        | Dead -> failwith "coroutine already dead"
        | Running k' ->
          corout.stat <- Dead;
          k' value
        | Suspended _ -> failwith "coroutine is suspended instead of running"
      in

      corout.stat <- Suspended corout_k;
      k @@ Coroutine corout
    | _ -> failwith "argument given to coroutine.create is not a function"
  end
  | CoroutResume -> begin
    eval_args env co args @@ fun args_evaluated ->
    let corout, arg =
      match args_evaluated with
      | [ corout ] -> (Value.as_coroutine corout, Value.Nil)
      | [ corout; arg ] -> (Value.as_coroutine corout, arg)
      | _ -> failwith "too many arguments given to coroutine.mini_resume"
    in

    match corout.stat with
    | Dead -> failwith "coroutine already dead"
    | Running _ -> failwith "coroutine is running instead of suspended"
    | Suspended k' ->
      corout.stat <- Running k;
      k' arg
  end
  | CoroutYield -> begin
    eval_args env co args @@ fun args_evaluated ->
    let arg =
      match args_evaluated with
      | [] -> Value.Nil
      | [ arg ] -> arg
      | _ -> failwith "too many arguments given to coroutine.yield"
    in

    match co.stat with
    | Dead -> failwith "coroutine already dead"
    | Running k' ->
      co.stat <- Suspended k;
      k' arg
    | Suspended _ -> failwith "coroutine is suspended instead of running"
  end
  | CoroutStatus -> begin
    eval_args env co args @@ fun args_evaluated ->
    let corout = List.hd args_evaluated |> Value.as_coroutine in

    let str =
      match corout.stat with
      | Dead -> "dead"
      | Running _ -> "running"
      | Suspended _ -> "suspended"
    in

    k @@ Value.String str
  end

and interp_exp (env : env) (co : coroutine) (e : exp) (k : value -> unit) : unit
    =
  match e with
  | Nil -> k Value.Nil
  | False -> k @@ Value.Bool false
  | True -> k @@ Value.Bool true
  | Integer n -> k @@ Value.Int n
  | Float f -> k @@ Value.Float f
  | LiteralString s -> k @@ Value.String s
  | Var (Name name) -> k @@ Value.lookup_ident env name
  | Var (IndexTable (table, key)) ->
    interp_exp env co table @@ fun value_tbl ->
    interp_exp env co key @@ fun value_k ->
    let table = Value.as_table value_tbl in
    let key = Value.as_table_key value_k in
    let value = Hashtbl.find_opt table key |> Option.value ~default:Value.Nil in
    k value
  | FunctionCallE f -> interp_funcall env co f k
  | FunctionDef fun_body ->
    let params, code = fun_body in
    let f = Value.Closure (params, env, code) in
    k @@ Value.Function f
  | BinOp (op, e1, e2) -> begin
    interp_exp env co e1 @@ fun v1 ->
    match op with
    (* logical operators *)
    | LogicalAnd -> if Value.as_bool v1 then interp_exp env co e2 k else k v1
    | LogicalOr -> if Value.as_bool v1 then k v1 else interp_exp env co e2 k
    (* other operators *)
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
        | Inequality -> Value.Bool (not @@ Value.equal v1 v2)
        | Less -> Value.Bool (Value.lt v1 v2)
        | Greater -> Value.Bool (not @@ Value.le v1 v2)
        | LessEq -> Value.Bool (Value.le v1 v2)
        | GreaterEq -> Value.Bool (not @@ Value.lt v1 v2)
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
  | Table items -> begin
    let table = Hashtbl.create 16 in

    let rec loop items =
      match items with
      | [] -> ()
      | (k, v) :: items' ->
        interp_exp env co k @@ fun key ->
        interp_exp env co v @@ fun value ->
        let key = Value.as_table_key key in
        Hashtbl.replace table key value;
        loop items'
    in

    loop items;
    k @@ Value.Table table
  end

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
