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
        // TODO: Utiliser les variables locales

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
            Stat_::Assign(var, exp) => todo!(),
            Stat_::WhileDoEnd(exp, stat) => todo!(),
            Stat_::If(exp, s1, s2) => todo!(),
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
            Function::Closure(_, _, _) => unimplemented!(),
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
            _ => unimplemented!(),
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
