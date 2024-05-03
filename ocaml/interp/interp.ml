open Luaparser.Ast

type value = Value.t

type env = Value.env

(* Fonction auxiliaire pour créer une table d'environnement à partir de noms et
   valeurs associées. *)
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

(* Fonctions de l'interprète, mutuellement récursives. Une fonction par
   catégorie syntaxique de l'AST. *)

(* Interprète un bloc de code *)
let rec interp_block (env : env) (blk : block) : value =
  let values = List.map (Fun.const Value.Nil) blk.locals in
  let local_scope = create_scope blk.locals values in

  let env = { env with locals = local_scope :: env.locals } in

  interp_stat env blk.body;
  interp_exp env blk.ret

(* Interprète un statement *)
and interp_stat (env : env) (stat : stat) : unit =
  match stat with
  | Nop -> ()
  | Seq (s1, s2) ->
    interp_stat env s1;
    interp_stat env s2
  | Assign (var, exp) -> begin
    match var with
    | Name name ->
      let value = interp_exp env exp in
      Value.set_ident env name value
    | IndexTable (tbl, k) ->
      let table = interp_exp env tbl |> Value.as_table in
      let key = interp_exp env k |> Value.as_table_key in
      let value = interp_exp env exp in
      Hashtbl.replace table key value
  end
  | FunctionCall f -> interp_funcall env f |> ignore
  | WhileDoEnd (exp, stat) ->
    while interp_exp env exp |> Value.as_bool do
      interp_stat env stat
    done
  | If (exp, s1, s2) ->
    if interp_exp env exp |> Value.as_bool then interp_stat env s1
    else interp_stat env s2

(* Interprète un appel de fonction *)
and interp_funcall (env : env) (fc : functioncall) : value =
  let f, args = fc in
  match interp_exp env f |> Value.as_function with
  | Print ->
    let () =
      List.map (fun exp -> interp_exp env exp |> Value.to_string) args
      |> String.concat "\t" |> Printf.printf "%s\n"
    in
    Value.Nil
  | Closure (params, local_env, block) ->
    let args_evaluated = List.map (interp_exp env) args in
    let local_scope = create_scope params args_evaluated in

    let env = { local_env with locals = local_scope :: local_env.locals } in
    interp_block env block

(* Interprète une expression *)
and interp_exp (env : env) (e : exp) : value =
  match e with
  | Nil -> Value.Nil
  | False -> Value.Bool false
  | True -> Value.Bool true
  | Integer n -> Value.Int n
  | Float f -> Value.Float f
  | LiteralString s -> Value.String s
  | Var (Name name) -> Value.lookup_ident env name
  | Var (IndexTable (tbl, k)) ->
    let table = interp_exp env tbl |> Value.as_table in
    let key = interp_exp env k |> Value.as_table_key in
    Hashtbl.find_opt table key |> Option.value ~default:Value.Nil
  | FunctionCallE f -> interp_funcall env f
  | FunctionDef fun_body ->
    let params, code = fun_body in
    let f = Value.Closure (params, env, code) in
    Value.Function f
  | BinOp (op, e1, e2) -> begin
    let v1 = interp_exp env e1 in
    match op with
    | LogicalAnd -> if Value.as_bool v1 then interp_exp env e2 else v1
    | LogicalOr -> if Value.as_bool v1 then v1 else interp_exp env e2
    | _ -> begin
      let v2 = interp_exp env e2 in
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
    end
  end
  | UnOp (op, exp) -> begin
    let value = interp_exp env exp in
    match op with
    | UnaryMinus -> Value.neg value
    | Not -> Value.Bool (Value.as_bool value |> not)
  end
  | Table items ->
    let htbl = Hashtbl.create 16 in

    List.iter
      (fun (key, value) ->
        let k = interp_exp env key |> Value.as_table_key in
        let v = interp_exp env value in
        Hashtbl.replace htbl k v )
      items;

    Value.Table htbl

let run ast =
  let globals = Hashtbl.create 47 in
  Hashtbl.add globals "print" (Value.Function Print);
  let env = Value.{ globals; locals = [] } in
  ignore (interp_block env ast)
