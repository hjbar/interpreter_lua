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
  match stat with
  | Nop -> k ()
  | Seq (s1, s2) ->
    interp_stat env s1 (fun _ -> interp_stat env s2 (fun _ -> k ()))
  | Assign (Name name, exp) -> failwith "interp_stat Assign Name"
  | Assign (IndexTable (tbl, k), exp) ->
    failwith "interp_stat Assign IndexTable"
  | FunctionCall f -> interp_funcall env f (fun _ -> ())
  | WhileDoEnd (exp, stat) -> failwith "interp_stat WhileDoEnd"
  | If (exp, s1, s2) -> failwith "interp_stat If"

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
  let value =
    match e with
    | Nil -> Value.Nil
    | False -> Value.Bool false
    | True -> Value.Bool true
    | Integer n -> Value.Int n
    | Float f -> Value.Float f
    | LiteralString s -> Value.String s
    | Var (Name name) -> Value.lookup_ident env name
    | Var (IndexTable (tbl, k)) -> failwith "interp_exp Var IndexTable"
    | FunctionCallE f -> failwith "interp_exp FunctionCallE"
    | FunctionDef fun_body -> failwith "interp_exp FunctionDef"
    | BinOp (op, e1, e2) -> failwith "interp_exp BinOp"
    | UnOp (op, exp) -> failwith "interp_exp UnOp"
    | Table items -> failwith "interp_exp Table"
  in
  k value

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
