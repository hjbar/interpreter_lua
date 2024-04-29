use self::{
    env::{Env, GEnv, LEnv},
    value::{Function, Value},
};
use crate::parser::ast::*;
use std::{collections::HashMap, rc::Rc};

mod env;
pub mod value;

impl Block {
    // Interprétation d'un bloc
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast> {
        let it = std::iter::repeat(Value::Nil).take(self.locals.len());
        env.locals.extend(&self.locals, it);

        self.body.interp(env);
        return self.ret.interp(env);
    }
}

impl Stat_ {
    // Interprétation d'une instruction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) {
        match self {
            Stat_::Nop => (), // TODO: Bizarre
            Stat_::Seq(s1, s2) => { s1.interp(env); s2.interp(env) }
            Stat_::StatFunctionCall(f) => { f.interp(env); }
            Stat_::Assign(var, exp) => {
                match var {
                    Var::Name(name) => { let val = exp.interp(env); env.set(name, val) }
                    Var::IndexTable(e1, e2) => todo!(),
                }
            }
            Stat_::WhileDoEnd(exp, stat) => while exp.interp(env).as_bool() { stat.interp(env) },
            Stat_::If(exp, s1, s2) => if exp.interp(env).as_bool() { s1.interp(env) } else { s2.interp(env) },
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

                Value::Function(Function::Print)
            }
            Function::Closure(params, local_env, block) => {
                let it = self.1.iter().map(|x| x.interp(env));
                let locals = local_env.extend(&params, it);

                let mut closure_env = Env { locals, globals: env.globals };
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
            Exp_::Var(v) => {
                match v {
                    Var::Name(name) => env.lookup(name),
                    Var::IndexTable(e1, e2) => todo!(),
                }
            }
            Exp_::ExpFunctionCall(f) => f.interp(env),
            Exp_::FunctionDef(fun_body) => {
                let f = Function::Closure(&fun_body.0, env.locals.clone(), &fun_body.1);
                Value::Function(f)
            }
            Exp_::BinOp(binop, e1, e2) => {
                match binop {
                    BinOp::Addition => { let n1 = e1.interp(env); let n2 = e2.interp(env); Value::add(n1, n2) }
                    BinOp::Subtraction => { let n1 = e1.interp(env); let n2 = e2.interp(env); Value::sub(n1, n2) }
                    BinOp::Multiplication => { let n1 = e1.interp(env); let n2 = e2.interp(env); Value::mul(n1, n2) }
                    /* relational operators */
                    BinOp::Equality => { let b1 = e1.interp(env); let b2 = e2.interp(env); Value::Bool(b1 == b2) },
                    BinOp::Inequality => { let b1 = e1.interp(env); let b2 = e2.interp(env); Value::Bool(b1 != b2) },
                    BinOp::Less => { let n1 = e1.interp(env); let n2 = e2.interp(env); Value::Bool(Value::lt(n1, n2)) }
                    BinOp::Greater => { let n1 = e1.interp(env); let n2 = e2.interp(env); Value::Bool(!Value::le(n1, n2)) }
                    BinOp::LessEq => { let n1 = e1.interp(env); let n2 = e2.interp(env); Value::Bool(Value::le(n1, n2)) }
                    BinOp::GreaterEq => { let n1 = e1.interp(env); let n2 = e2.interp(env); Value::Bool(!Value::lt(n1, n2)) }
                    /* logical operators */
                    BinOp::LogicalAnd => { let b1 = e1.interp(env).as_bool(); let b2 = e2.interp(env).as_bool(); Value::Bool(b1 && b2) },
                    BinOp::LogicalOr => { let b1 = e1.interp(env).as_bool(); let b2 = e2.interp(env).as_bool(); Value::Bool(b1 || b2) },
                }
            }
            Exp_::UnOp(unop, exp) => {
                match unop {
                    UnOp::Not => Value::Bool(!exp.interp(env).as_bool()),
                    UnOp::UnaryMinus => exp.interp(env).neg(),
                }
            }
            Exp_::Table(vec) => todo!(),
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
