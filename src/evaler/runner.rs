use super::function::Function;
use super::module;
use super::r#type::Value;
use crate::assume;
use crate::evaler::runner::operator::{Operator, Operators};
use crate::parser::tokenizer::token::{Key, Token};
use std::sync::Arc;

pub mod operator;
pub mod rustbridge;
pub mod tokenbuffer;
use tokenbuffer::{SimpleBuffer, TokenBuffer, WhereBuffer};

pub struct Runner<'p> {
    params: &'p [Value],
    wheres: Arc<Vec<Vec<Token>>>,
    r_gp: Value,

    // TODO: I need to be able to store a list here. But using a Vec to forcefully heap-allocate
    // when I know that most of the time it'll only be 0-1 values in here seems like a bad idea.
    // I suggest we add a `UnsafeBuffer(Vec<Type>)` variant to the Value struct, which is used
    // internally. Point of it being that it doesn't hold type information, contains different
    // types, and bypasses type checking
    r_gp_secondary: Value,
}

enum EvalResult {
    // Put the value into the r_gp registrer
    Load(Value),
    // Apply operator to r_gp and next value
    Operator(Operators),
    // Call a native rust function
    BridgedFunctionCall(rustbridge::FunctionID),
    // Call a leaf function
    FunctionCall(Function),
    // Tail recursion optimization
    Recurse,
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

impl<'p> Runner<'p> {
    pub fn run(func: &'p mut Function, params: &'p [Value]) -> Value {
        let mut runner = Runner {
            params,
            wheres: func.wheres.clone(),
            r_gp: Value::Nothing,
            r_gp_secondary: Value::Nothing,
        };

        runner.run_buf(func)
    }

    fn eval(&mut self, token: &Token) -> EvalResult {
        match token {
            // Inline values such as strings and numbers
            Token::V(v) => EvalResult::Load(v.clone()),
            // Usage of a function parameter
            Token::ParamValue(v) => EvalResult::Load(self.params[*v].clone()),
            // Tail recursion
            Token::Recurse => EvalResult::Recurse,
            // Usage of a where statement
            Token::WhereValue(whereid) => {
                let buf = WhereBuffer::new(self.wheres.clone(), *whereid);
                EvalResult::Load(self.run_buf(buf))
            }
            // Lambda expression
            Token::Lambda(tbuf) => {
                let buf = SimpleBuffer::new(tbuf);
                self.r_gp_secondary = self.r_gp.take();
                self.r_gp = Value::Nothing;
                EvalResult::Load(self.run_buf(buf))
            }
            // Tokens that together result in one value. For example the content between parenthesis
            Token::Group(tbuf) => {
                let buf = SimpleBuffer::new(tbuf);
                EvalResult::Load(self.run_buf(buf))
            }
            Token::RuntimeList(entities) => {
                let mut v_list: Vec<Value> = Vec::with_capacity(entities.len());

                for entity in entities {
                    // TODO I should be able to get rid of clone here
                    let v = self.run_buf(SimpleBuffer::new(&[entity.clone()]));
                    v_list.push(v);
                }
                EvalResult::Load(Value::List(v_list))
            }
            // Function calls
            Token::Function(fid, funcid) => {
                let func = module::get(*fid).unwrap().get_function(*funcid);
                EvalResult::FunctionCall(func)
            }
            // rust:call<n> functions
            Token::BridgedFunction(bfid) => EvalResult::BridgedFunctionCall(*bfid),
            Token::IfStatement(branches, else_do) => {
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
            // Operators
            Token::Key(Key::Operator(op)) => EvalResult::Operator(op.clone()),
            _ => panic!("{}", &token),
        }
    }

    fn run_buf<B: TokenBuffer>(&mut self, mut tbuf: B) -> Value {
        loop {
            let token = match tbuf.next_token() {
                Some(t) => t,
                None => return self.r_gp.take(),
            };
            match self.eval(token) {
                EvalResult::Load(v) => self.r_gp = v,
                EvalResult::Operator(op) => {
                    let left = self.r_gp.take();
                    let right = self.get_params(&mut tbuf, 1);
                    self.r_gp = op.operate(&left, &right[0]);
                }
                EvalResult::Recurse => {
                    tbuf.reset();
                    self.r_gp = Value::Nothing;
                }
                EvalResult::FunctionCall(mut func) => {
                    let params = self.get_params(&mut tbuf, func.params as usize);
                    self.r_gp = Runner::run(&mut func, &params);
                }
                //EvalResult::Error(e) => return Err(e),
                EvalResult::BridgedFunctionCall(bfid) => return self.run_bridged(&mut tbuf, bfid),
            }
        }
    }

    fn run_bridged<B: TokenBuffer>(&mut self, tbuf: &mut B, bfid: rustbridge::FunctionID) -> Value {
        let param = self.get_param(tbuf);
        let f = rustbridge::get_function(bfid);
        f(param)
    }

    fn get_params<B: TokenBuffer>(&mut self, tbuf: &mut B, amount: usize) -> Vec<Value> {
        let mut params = Vec::with_capacity(amount);
        for _i in 0..amount {
            params.push(self.get_param(tbuf));
        }
        params
    }

    fn get_param<B: TokenBuffer>(&mut self, tbuf: &mut B) -> Value {
        match self.eval(tbuf.next_token().unwrap()) {
            EvalResult::Load(v) => v,
            EvalResult::Recurse => panic!("Impossible tail recursion"),
            EvalResult::FunctionCall(mut nested_func) => {
                let nested_params = self.get_params(tbuf, nested_func.params as usize);
                Runner::run(&mut nested_func, &nested_params)
            }
            EvalResult::BridgedFunctionCall(bfid) => self.run_bridged(tbuf, bfid),
            EvalResult::Operator(op) => panic!(
                "Attempted to use standalone operator as parameter without wrapper ({:?})",
                op
            ),
        }
    }
}
