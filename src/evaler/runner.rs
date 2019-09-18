use super::function::Function;
use super::module;
use super::r#type::Value;
use crate::assume;
use crate::evaler::runner::operator::Operator;
use std::sync::Arc;

mod entity;
pub use entity::Entity;

pub mod operator;
pub mod rustbridge;
pub mod tokenbuffer;
use tokenbuffer::{SimpleBuffer, TokenBuffer, WhereBuffer};

pub struct Runner {
    params: Vec<Value>,
    wheres: Arc<Vec<Vec<Entity>>>,
    r_gp: Value,
}

enum EvalResult {
    // Put the value into the r_gp registrer
    Load(Value),
    // Call a native rust function
    BridgedFunctionCall(rustbridge::FunctionID, Vec<Value>),
    // Call a leaf function
    FunctionCall(Function, Vec<Value>),
    // Tail recursion optimization
    Recurse(Vec<Value>),
}

/* GUIDE
 *
 * the run_buf, run_bridged etc are roots of functions/groups. They go through all the internal
 * tokens and calls .eval() on them.
 *
 * the self.eval function takes the active token and runs it. Sometimes this just drops a value
 * such as if it's a Token::V(V::Int(10)), if that's the case it'll return an EvalResult::Load(v) to
 * the parent run_buf/run_bridged. Sometimes it can be another function call. If that's the case
 * it'll first recursively gather parameters, then construct a new Runner, and return that Runner
 * as an EvalResult::FunctionCall(fid, funcid).
 *
 * Lets use this Leaf code as example:
 *   add (20 + 20) << 10 + 10
 *
 * It'll then construct an EvalResult::FunctionCall(Function) by fetching the Function `add` from the global static module::MODULES
 * (using its file and function IDs which replace the string identifier at parse-time)
 * And return that to the parent run_buf/run_bridged.
 *
 * run_buf/run_bridged then starts gathering parameters by calling self.eval() repeatadly
 * untill the expected amount is reached.
 *
 * The first eval() call would in this example discover the token (in simplified IR):
 * Token::Group([V(20), Operator::Add, V(20)])
 * This would call run_buf on the contents of that group and eventually result in a EvalResult::Load(V(40))
 *
 * the second one will discover the token:
 * Token::Group([V(10), Operator::Add, V(10)])
 * And then do the same as above
 *
 * Once everything's gathered for the function to run, run_buf/run_bridged constructs a new
 * nested Runner, passing the Function and the parameters, and then runs it.
 *
 * Basically the entire program is a bunch of nested Runners. A function and a token group such as
 * the contents inbetween parenthesis are treated the same way but with different token buffers.
 */

impl Runner {
    pub fn run(func: &mut Function, params: Vec<Value>) -> Value {
        let mut runner = Runner {
            params,
            wheres: func.wheres.clone(),
            r_gp: Value::Nothing,
        };

        runner.run_buf(func)
    }

    fn run_buf<B: TokenBuffer>(&mut self, mut tbuf: B) -> Value {
        loop {
            let token = match tbuf.next_entity() {
                Some(t) => t,
                None => return self.r_gp.take(),
            };
            match self.eval(token) {
                EvalResult::Load(v) => self.r_gp = v,
                EvalResult::Recurse(params) => {
                    tbuf.reset();
                    self.params = params;
                    self.r_gp = Value::Nothing;
                }
                EvalResult::FunctionCall(mut func, params) => {
                    self.r_gp = Runner::run(&mut func, params);
                }
                EvalResult::BridgedFunctionCall(bfid, params) => {
                    self.r_gp = {
                        let f = rustbridge::get_function(bfid);
                        f(&params)
                    };
                }
            }
        }
    }

    fn eval(&mut self, entity: &Entity) -> EvalResult {
        match entity {
            // Inline values such as strings and numbers
            Entity::V(v) => EvalResult::Load(v.clone()),
            // Usage of a function parameter
            Entity::ParamValue(v) => EvalResult::Load(self.params[*v].clone()),
            // Tail recursion
            Entity::Recurse(params) => {
                let mut evaluated_params = Vec::with_capacity(params.len());
                for p in params.iter() {
                    match self.eval(p) {
                        EvalResult::Load(v) => {
                            evaluated_params.push(v);
                        }
                        EvalResult::Recurse(_) => panic!("Impossible tail recursion"),
                        EvalResult::FunctionCall(mut func, params) => {
                            assert_eq!(params.len(), 0);
                            evaluated_params.push(Runner::run(&mut func, Vec::new()));
                        }
                        EvalResult::BridgedFunctionCall(bfid, params) => {
                            assert_eq!(params.len(), 0);
                            let f = rustbridge::get_function(bfid);
                            evaluated_params.push(f(&[]));
                        }
                    }
                }
                EvalResult::Recurse(evaluated_params)
            }
            // Usage of a where statement
            Entity::WhereValue(whereid) => {
                let buf = WhereBuffer::new(self.wheres.clone(), *whereid);
                EvalResult::Load(self.run_buf(buf))
            }
            // Tokens that together result in one value. For example the content between parenthesis
            Entity::Group(tbuf) => {
                let buf = SimpleBuffer::new(tbuf);
                EvalResult::Load(self.run_buf(buf))
            }
            Entity::EvaluationList(entities) => {
                let mut v_list: Vec<Value> = Vec::with_capacity(entities.len());

                for entity in entities {
                    // TODO I should be able to get rid of clone here
                    let v = self.run_buf(SimpleBuffer::new(&[entity.clone()]));
                    v_list.push(v);
                }
                EvalResult::Load(Value::List(v_list))
            }
            // Function calls
            Entity::Function(fid, funcid, params) => {
                let func = module::get(*fid).unwrap().get_function(*funcid);
                let mut evaluated_params = Vec::with_capacity(params.len());
                for p in params.iter() {
                    match self.eval(p) {
                        EvalResult::Load(v) => {
                            evaluated_params.push(v);
                        }
                        EvalResult::Recurse(_) => panic!("Impossible tail recursion"),
                        EvalResult::FunctionCall(mut func, params) => {
                            assert_eq!(params.len(), 0);
                            evaluated_params.push(Runner::run(&mut func, Vec::new()));
                        }
                        EvalResult::BridgedFunctionCall(bfid, params) => {
                            assert_eq!(params.len(), 0);
                            let f = rustbridge::get_function(bfid);
                            evaluated_params.push(f(&[]));
                        }
                    }
                }
                EvalResult::FunctionCall(func, evaluated_params)
            }
            // rust:call<n> functions
            Entity::BridgedFunction(bfid, params) => {
                let mut evaluated_params = Vec::with_capacity(params.len());
                for p in params.iter() {
                    match self.eval(p) {
                        EvalResult::Load(v) => {
                            evaluated_params.push(v);
                            break;
                        }
                        EvalResult::Recurse(_) => panic!("Impossible tail recursion"),
                        EvalResult::FunctionCall(mut func, params) => {
                            assert_eq!(params.len(), 0);
                            evaluated_params.push(Runner::run(&mut func, Vec::new()));
                            break;
                        }
                        EvalResult::BridgedFunctionCall(bfid, params) => {
                            assert_eq!(params.len(), 0);
                            let f = rustbridge::get_function(bfid);
                            evaluated_params.push(f(&[]));
                            break;
                        }
                    }
                }
                EvalResult::BridgedFunctionCall(*bfid, evaluated_params)
            }
            Entity::IfStatement(branches, else_do) => {
                for branch in branches.iter() {
                    if assume!(self.run_buf(SimpleBuffer::new(&branch.0)), Value::Bool) {
                        let v = self.run_buf(SimpleBuffer::new(&branch.1));
                        return EvalResult::Load(v);
                    }
                }
                // None of the conditions were met, lets run the else statement
                let v = self.run_buf(SimpleBuffer::new(&else_do));
                EvalResult::Load(v)
            }
            Entity::Operation(left_right, op) => {
                //let deref = *left_right;
                let mut eval = |e| -> Value {
                    match self.eval(e) {
                        EvalResult::Load(v) => v,
                        EvalResult::Recurse(_) => panic!("Impossible tail recursion"),
                        EvalResult::FunctionCall(mut func, params) => {
                            Runner::run(&mut func, params)
                        }
                        EvalResult::BridgedFunctionCall(bfid, params) => {
                            let f = rustbridge::get_function(bfid);
                            f(&params)
                        }
                    }
                };
                let left = eval(&left_right.0);
                let right = eval(&left_right.1);
                EvalResult::Load(op.operate(&left, &right))
            }
            _ => panic!("{}", &entity),
        }
    }
}
