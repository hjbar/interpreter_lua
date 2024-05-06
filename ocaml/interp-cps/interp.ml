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

let rec interp_block (env : env) (co : coroutine) (blk : block)
  (k : value -> unit) : unit =
  (* le padding avec Value.Nil est geré par create_scope *)
  let local_scope = create_scope blk.locals [] in
  let env = { env with locals = local_scope :: env.locals } in

  interp_stat env co blk.body (fun _ ->
      interp_exp env co blk.ret (fun value -> k value) )

and interp_stat (env : env) (co : coroutine) (stat : stat) (k : unit -> unit) :
  unit =
  let stat =
    match stat with
    | Nop -> ()
    | Seq (s1, s2) ->
      interp_stat env co s1 (fun _ -> interp_stat env co s2 (fun _ -> ()))
    | Assign (Name name, exp) ->
      interp_exp env co exp (fun value -> Value.set_ident env name value)
    | Assign (IndexTable (table, key), exp) ->
      interp_exp env co table (fun table_v ->
          interp_exp env co key (fun key_v ->
              interp_exp env co exp (fun value ->
                  let table = Value.as_table table_v in
                  let key = Value.as_table_key key_v in
                  Hashtbl.replace table key value ) ) )
    | FunctionCall f -> interp_funcall env co f (fun _ -> ())
    | WhileDoEnd (exp, stat) -> while_do_end env co exp stat
    | If (exp, s1, s2) ->
      interp_exp env co exp (fun value ->
          if Value.as_bool value then interp_stat env co s1 (fun _ -> ())
          else interp_stat env co s2 (fun _ -> ()) )
  in
  k stat

and while_do_end env co exp stat =
  interp_exp env co exp (fun value ->
      if Value.as_bool value then
        interp_stat env co stat (fun _ -> while_do_end env co exp stat) )

and interp_funcall (env : env) (co : coroutine) (fc : functioncall)
  (k : value -> unit) : unit =
  let f, args = fc in
  interp_exp env co f (fun func ->
      let func = Value.as_function func in
      match func with
      (* fonctions avec k interne *)
      | Closure (params, local_env, block) ->
        (* la possible différence de longueur entre les
           paramètres et les arguments est geré par create_scope *)
        let args_evaluated = eval_args env co args in
        let local_scope = create_scope params args_evaluated in

        let env = { local_env with locals = local_scope :: local_env.locals } in
        interp_block env co block (fun value -> k value)
      (* fonctions avec k externe *)
      | _ -> begin
        let value =
          match func with
          | Print ->
            print env co args;
            Value.Nil
          | CoroutCreate ->
            Printf.printf "Entre dans CoroutCreate\n%!";
            let args_evaluated = eval_args env co args in
            let f = List.hd args_evaluated in
            let corout =
              Value.Coroutine Value.{ stat = Value.Suspended (fun _ -> k f) }
            in
            corout
          | CoroutResume ->
            Printf.printf "Entre dans CoroutResume\n%!";
            let () =
              let f =
                match co.stat with
                | Dead -> failwith "The current coroutine is dead"
                | Running f -> f
                | Suspended f ->
                  co.stat <- Running f;
                  f
              in
              f (Value.Coroutine co)
            in
            Value.Coroutine co
          | CoroutYield ->
            Printf.printf "Entre dans CoroutYield\n%!";
            let () =
              match co.stat with
              | Dead -> failwith "The current coroutine is dead"
              | Running f -> co.stat <- Suspended f
              | Suspended _ -> ()
            in
            Value.Coroutine co
          | CoroutStatus ->
            Printf.printf "Entre dans CoroutStatus\n%!";
            let str =
              match co.stat with
              | Dead -> "dead"
              | Suspended _ -> "suspended"
              | Running _ -> "running"
            in
            Value.String str
          | _ -> assert false
        in
        k value
      end )

and eval_args (env : env) (co : coroutine) (args : args) : value list =
  let res = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | exp :: l' ->
      interp_exp env co exp (fun value ->
          res := value :: !res;
          loop l' )
  in
  loop args;
  List.rev !res

and print (env : env) (co : coroutine) (args : args) : unit =
  match args with
  | [] -> ()
  | [ exp ] ->
    interp_exp env co exp (fun value ->
        Printf.printf "%s\n" (value |> Value.to_string) )
  | exp :: args' ->
    interp_exp env co exp (fun value ->
        Printf.printf "%s\t" (value |> Value.to_string);
        print env co args' )

and interp_exp (env : env) (co : coroutine) (e : exp) (k : value -> unit) : unit
    =
  match e with
  (* expressions avec k interne *)
  | Var (IndexTable (table, key)) ->
    interp_exp env co table (fun value_tbl ->
        interp_exp env co key (fun value_k ->
            let table = Value.as_table value_tbl in
            let key = Value.as_table_key value_k in
            let value =
              Hashtbl.find_opt table key |> Option.value ~default:Value.Nil
            in
            k value ) )
  | FunctionCallE f -> interp_funcall env co f (fun value -> k value)
  | BinOp (op, e1, e2) ->
    interp_exp env co e1 (fun v1 ->
        match op with
        | LogicalAnd ->
          if Value.as_bool v1 then interp_exp env co e2 (fun v2 -> k v2)
          else k v1
        | LogicalOr ->
          if Value.as_bool v1 then k v1
          else interp_exp env co e2 (fun v2 -> k v2)
        | _ ->
          interp_exp env co e2 (fun v2 ->
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
    interp_exp env co exp (fun value ->
        let value' =
          match op with
          | UnaryMinus -> Value.neg value
          | Not -> Value.Bool (Value.as_bool value |> not)
        in
        k value' )
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
    interp_exp env co k (fun key ->
        interp_exp env co v (fun value ->
            let key = Value.as_table_key key in
            Hashtbl.replace table key value;
            add_items env co table items' ) )

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
  let co =
    Value.Coroutine Value.{ stat = Value.Running (fun _ -> ()) }
    |> Value.as_coroutine
  in
  interp_block env co ast (fun _ -> ())
