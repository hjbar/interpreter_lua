open Luaparser.Ast

type value = Value.t

type coroutine = Value.coroutine

type env = Value.env

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

let rec interp_block (env : env) (blk : block) (k : value -> unit) : unit =
  (* le padding avec Value.Nil est geré par create_scope *)
  let local_scope = create_scope blk.locals [] in
  let env = { env with locals = local_scope :: env.locals } in

  interp_stat env blk.body (fun _ ->
      interp_exp env blk.ret (fun value -> k value) )

and interp_stat (env : env) (stat : stat) (k : unit -> unit) : unit =
  let stat =
    match stat with
    | Nop -> ()
    | Seq (s1, s2) ->
      interp_stat env s1 (fun _ -> interp_stat env s2 (fun _ -> ()))
    | Assign (Name name, exp) ->
      interp_exp env exp (fun value -> Value.set_ident env name value (*|> k*))
    | Assign (IndexTable (tbl, k), exp) ->
      failwith "interp_stat Assign IndexTable"
    | FunctionCall f -> interp_funcall env f (fun _ -> ())
    | WhileDoEnd (exp, stat) -> failwith "interp_stat WhileDoEnd"
    | If (exp, s1, s2) -> failwith "interp_stat If"
  in
  k stat

and interp_funcall (env : env) (fc : functioncall) (k : value -> unit) : unit =
  let f, args = fc in
  interp_exp env f (fun res ->
      let value =
        match Value.as_function res with
        | Closure (params, local_env, block) ->
          failwith "interp_funcall Closure"
        | Print ->
          let rec loop l =
            match l with
            | [] -> ()
            | [ exp ] ->
              interp_exp env exp (fun value ->
                  Printf.printf "%s\n" (value |> Value.to_string) )
            | exp :: l' ->
              interp_exp env exp (fun value ->
                  Printf.printf "%s\t" (value |> Value.to_string) );
              loop l'
          in
          loop args;
          Value.Nil
        | CoroutCreate -> failwith "interp_funcall CoroutCreate"
        | CoroutResume -> failwith "interp_funcall CoroutResume"
        | CoroutYield -> failwith "interp_funcall CoroutYield"
        | CoroutStatus -> failwith "interp_funcall CoroutStatus"
      in
      k value )

and interp_exp (env : env) (e : exp) (k : value -> unit) : unit =
  match e with
  | Nil -> Value.Nil |> k
  | False -> Value.Bool false |> k
  | True -> Value.Bool true |> k
  | Integer n -> Value.Int n |> k
  | Float f -> Value.Float f |> k
  | LiteralString s -> Value.String s |> k
  | Var (Name name) -> Value.lookup_ident env name |> k
  | Var (IndexTable (table, key)) ->
    interp_exp env table (fun value_tbl ->
        interp_exp env key (fun value_k ->
            let table = Value.as_table value_tbl in
            let key = Value.as_table_key value_k in
            let value =
              Hashtbl.find_opt table key |> Option.value ~default:Value.Nil
            in
            k value ) )
  | FunctionCallE f -> interp_funcall env f (fun value -> k value)
  | FunctionDef fun_body ->
    let params, code = fun_body in
    let f = Value.Closure (params, env, code) in
    Value.Function f |> k
  | BinOp (op, e1, e2) ->
    interp_exp env e1 (fun v1 ->
        match op with
        | LogicalAnd ->
          if Value.as_bool v1 then interp_exp env e2 (fun v2 -> k v2) else k v1
        | LogicalOr ->
          if Value.as_bool v1 then k v1 else interp_exp env e2 (fun v2 -> k v2)
        | _ ->
          interp_exp env e2 (fun v2 ->
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
              k value ) )
  | UnOp (op, exp) ->
    interp_exp env exp (fun value ->
        let value' =
          match op with
          | UnaryMinus -> Value.neg value
          | Not -> Value.Bool (Value.as_bool value |> not)
        in
        k value' )
  | Table items -> failwith "interp_exp Table"

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
  interp_block env ast (fun _ -> ())
