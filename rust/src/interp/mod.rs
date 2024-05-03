use self::{
    env::{Env, GEnv, LEnv},
    value::{Function, Value},
};
use crate::parser::ast::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

mod env;
pub mod value;

impl Block {
    // Interprétation d'un bloc
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        // le padding avec Value::Nil est geré par extend
        let it = std::iter::empty();
        let locals = env.locals.extend(&self.locals, it);

        let mut env = Env {
            locals,
            globals: env.globals,
        };

        self.body.interp(&mut env);
        self.ret.interp(&mut env)
    }
}

impl Stat_ {
    // Interprétation d'une instruction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) {
        match self {
            Stat_::Nop => (),

            Stat_::Seq(s1, s2) => {
                s1.interp(env);
                s2.interp(env)
            }

            Stat_::StatFunctionCall(f) => {
                f.interp(env);
            }

            Stat_::Assign(Var::Name(name), exp) => {
                let val = exp.interp(env);
                env.set(name, val)
            }

            Stat_::Assign(Var::IndexTable(tbl, k), exp) => {
                let table = tbl.interp(env).as_table();
                let key = k.interp(env).as_table_key();
                let val = exp.interp(env);

                table.clone().borrow_mut().insert(key, val);
            }

            Stat_::WhileDoEnd(exp, stat) => {
                while exp.interp(env).as_bool() {
                    stat.interp(env)
                }
            }

            Stat_::If(exp, s1, s2) => {
                if exp.interp(env).as_bool() {
                    s1.interp(env)
                } else {
                    s2.interp(env)
                }
            }
        }
    }
}

impl FunctionCall {
    // Interprétation d'un appel de fonction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        match self.0.interp(env).as_function() {
            Function::Print => {
                let last_index = self.1.len() - 1;

                for i in 0..last_index {
                    print!("{}\t", self.1[i].interp(env));
                }
                print!("{}\n", self.1[last_index].interp(env));

                Value::Nil
            }

            Function::Closure(params, local_env, block) => {
                // la possible différence de longueur entre les
                // paramètres et les arguments est geré par extend
                let it = self.1.iter().map(|exp| exp.interp(env));
                let locals = local_env.extend(params, it);

                let mut closure_env = Env {
                    locals,
                    globals: env.globals,
                };

                block.interp(&mut closure_env)
            }
        }
    }
}

impl Exp_ {
    // Interprétation d'une expression
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        match self {
            Exp_::Nil => Value::Nil,

            Exp_::False => Value::Bool(false),

            Exp_::True => Value::Bool(true),

            Exp_::Number(n) => Value::Number(*n),

            Exp_::LiteralString(s) => Value::String(s.clone()),

            Exp_::Var(Var::Name(name)) => env.lookup(name),

            Exp_::Var(Var::IndexTable(tbl, k)) => {
                let table = tbl.interp(env).as_table();
                let key = k.interp(env).as_table_key();

                match table.clone().borrow().get(&key) {
                    None => Value::Nil,
                    Some(res) => res.clone(),
                }
            }

            Exp_::ExpFunctionCall(f) => f.interp(env),

            Exp_::FunctionDef(fun_body) => {
                let f = Function::Closure(&fun_body.0, env.locals.clone(), &fun_body.1);
                Value::Function(f)
            }

            Exp_::BinOp(binop, e1, e2) => {
                let v1 = e1.interp(env);

                if let BinOp::LogicalAnd = binop {
                    return if v1.as_bool() { e2.interp(env) } else { v1 };
                }

                if let BinOp::LogicalOr = binop {
                    return if v1.as_bool() { v1 } else { e2.interp(env) };
                }

                let v2 = e2.interp(env);

                match binop {
                    /* arithmetic operators */
                    BinOp::Addition => v1.add(v2),
                    BinOp::Subtraction => v1.sub(v2),
                    BinOp::Multiplication => v1.mul(v2),
                    /* relational operators */
                    BinOp::Equality => Value::Bool(v1 == v2),
                    BinOp::Inequality => Value::Bool(v1 != v2),
                    BinOp::Less => Value::Bool(v1.lt(v2)),
                    BinOp::Greater => Value::Bool(!v1.le(v2)),
                    BinOp::LessEq => Value::Bool(v1.le(v2)),
                    BinOp::GreaterEq => Value::Bool(!v1.lt(v2)),
                    /* logical operators */
                    _ => unreachable!(),
                }
            }

            Exp_::UnOp(unop, exp) => {
                let v = exp.interp(env);
                match unop {
                    UnOp::Not => Value::Bool(!v.as_bool()),
                    UnOp::UnaryMinus => v.neg(),
                }
            }

            Exp_::Table(items) => {
                let mut table = HashMap::new();

                for (key, val) in items {
                    let k = key.interp(env).as_table_key();
                    let v = val.interp(env);
                    table.insert(k, v);
                }

                Value::Table(Rc::new(RefCell::new(table)))
            }
        }
    }
}

// Point d'entrée principal de l'interpréteur
pub fn run(ast: &Block) {
    let mut globals = GEnv(HashMap::new());
    let printid = "print".to_owned();
    globals.0.insert(&printid, Value::Function(Function::Print));
    let mut env = Env {
        locals: Rc::new(LEnv::Nil),
        globals: &mut globals,
    };
    ast.interp(&mut env);
}
