use crate::InterResolved;

use super::*;
use lumina_parser as par;

#[derive(Clone, Debug)]
pub struct OpTree<'a, 's> {
    func: ExprId,
    precedence: u32,
    sides: Box<[Side<'a, 's>; 2]>,
}

impl<'a, 's> Side<'a, 's> {
    fn handle_right(self, precedence: u32, op: ExprId, rhs: Tr<&'a par::Expr<'s>>) -> Side<'a, 's> {
        match self {
            Side::Op(mut lop) => {
                if precedence > lop.precedence {
                    let previous_rhs = lop.sides[1].clone();
                    let this = OpTree {
                        func: op,
                        precedence,
                        sides: Box::new([previous_rhs, Side::Tail(rhs)]),
                    };
                    lop.sides[1] = Side::Op(this);
                    Side::Op(lop)
                } else {
                    let this = OpTree {
                        func: op,
                        precedence,
                        sides: Box::new([Side::Op(lop), Side::Tail(rhs)]),
                    };
                    Side::Op(this)
                }
            }
            Side::Tail(lhs) => Side::Op(OpTree {
                func: op,
                precedence,
                sides: Box::new([Side::Tail(lhs), Side::Tail(rhs)]),
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Side<'a, 's> {
    Op(OpTree<'a, 's>),
    Tail(Tr<&'a par::Expr<'s>>),
}

impl<'a, 's> FuncLower<'a, 's> {
    pub fn handle_operator<'e>(
        &mut self,
        lhs: Side<'e, 's>,
        op: Tr<&'s str>,
        rhs: Tr<&'e par::Expr<'s>>,
    ) -> Side<'e, 's> {
        const BOOL_AND_PREC: u32 = 2000;
        const BOOL_OR_PREC: u32 = 1900;

        match *op {
            // Since we want these to be lazy, they can't be user-defined operators
            "&&" => {
                let f = self.applyable_builtin(op.span, vec!["bool", "and"], &[]);
                lhs.handle_right(BOOL_AND_PREC, f, rhs)
            }
            "||" => {
                let f = self.applyable_builtin(op.span, vec!["bool", "or"], &[]);
                lhs.handle_right(BOOL_OR_PREC, f, rhs)
            }

            _ => match self.find(symbols::Namespace::Functions, &[*op]) {
                InterResolved::Item(origin, symbols::Item::Func(func)) => {
                    if let symbols::Origin::Intra = origin {
                        match self.fork(func).ensure_function_is_lowered(false) {
                            func::LowerResult::CircularInference => {
                                todo!("ET: circular error");
                            }
                            _ => {}
                        }
                    }

                    let precedence = self.ctx().in_origin(self.project(), origin, |unit| {
                        unit.header.func(func).precedence
                    });

                    let fvar = self
                        .tctx
                        .lowering_instantiation()
                        .operator(op.span, origin, func);

                    let f = self.add_typed_expr(Expr::Func(origin, func), fvar, op.span);
                    lhs.handle_right(precedence, f, rhs)
                }
                InterResolved::Item(origin, symbols::Item::Method(trait_, method)) => {
                    let (precedence, func) = self.ctx().in_origin(self.project(), origin, |unit| {
                        let func = unit.header.method(trait_, method);
                        (unit.header.func(func).precedence, func)
                    });

                    let fvar = self
                        .tctx
                        .lowering_instantiation()
                        .operator(op.span, origin, func);

                    let f = self.add_typed_expr(Expr::Func(origin, func), fvar, op.span);
                    lhs.handle_right(precedence, f, rhs)
                }
                InterResolved::Module(_) => todo!(),
                InterResolved::Item(origin, item) => {
                    panic!("non-func as an operator: {origin}:{item:?}")
                }
                InterResolved::Builtin(_) => panic!("builtin as an operator"),
                InterResolved::NotFound { .. } => {
                    todo!("operator not found");
                }
                InterResolved::Poison => {
                    todo!("maybe we should use ExprId in side instead now that we apply?")
                }
            },
        }
    }

    pub fn fold_optree<'o>(&mut self, op: OpTree<'o, 's>) -> ExprId {
        let [left, right] = *op.sides;
        let left = self.fold_optree_side(left);
        let right = self.fold_optree_side(right);

        let mut params = key::EntityList::new();
        params.push(left, self.uarena.expr_pool_mut(self.id));
        params.push(right, self.uarena.expr_pool_mut(self.id));
        let span = self.uarena.exprs[left]
            .span
            .extend(self.uarena.exprs[right].span);

        self.apply_expr(span, op.func, params)
    }

    pub fn fold_optree_side<'o>(&mut self, side: Side<'o, 's>) -> ExprId {
        match side {
            Side::Op(op) => self.fold_optree(op),
            Side::Tail(edge) => self.expr(edge),
        }
    }
}
