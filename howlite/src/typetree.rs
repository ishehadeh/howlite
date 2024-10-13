use std::mem::MaybeUninit;

use howlite_syntax::{
    tree::{AssociatedTreeBuildContext, NodeId, PartialAssociatedTree, Tree},
    AstNode,
};
use howlite_typecheck::{
    errors::{IncompatibleError, OperationError},
    types::TyInt,
    Ty,
};
use sunstone::multi::DynSet;

use crate::symtab::{OwnedSymbolTable, Symbol};

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub ty_assumed: Option<Ty<Symbol>>,
    pub ty_synthesized: Ty<Symbol>,
    pub error: Option<TypeError>,
    pub inherited_errors: smallvec::SmallVec<[NodeId<AstNode>; 4]>,
}

impl TypeInfo {
    pub fn new(value_ty: Ty<Symbol>) -> Self {
        Self {
            ty_assumed: None,
            ty_synthesized: value_ty,
            error: None,
            inherited_errors: smallvec::SmallVec::new(),
        }
    }

    pub fn synthesize_from<const N: usize, F>(infos: [&TypeInfo; N], f: F)
    where
        F: FnOnce([&Ty<Symbol>; N]) -> Result<Ty<Symbol>, (Ty<Symbol>, TypeError)>,
    {
        let mut synth_tys: [&Ty<Symbol>; N] = [&Ty::Hole; N];
        for i in 0..N {
            synth_tys[i] = &infos[i].ty_synthesized
        }

        let ty_result = f(synth_tys);
        match ty_result {
            Ok(synth) => TypeInfo {
                ty_assumed: None,
                ty_synthesized: synth,
                error: None,
                inherited_errors: infos
                    .map(|t| t.inherited_errors.iter().cloned())
                    .flat()
                    .collect(),
            },
            Err((synth, error)) => TypeInfo {
                ty_assumed: None,
                ty_synthesized: synth,
                error: Some(error),
                inherited_errors: infos
                    .map(|t| t.inherited_errors.iter().cloned())
                    .flat()
                    .collect(),
            },
        }
    }
}

pub struct TypeTreeBuilder<'a> {
    ast: &'a Tree<AstNode>,
    type_tree: PartialAssociatedTree<'a, TypeInfo, AstNode>,
    pub symtab: OwnedSymbolTable,
}

impl<'a> TypeTreeBuilder<'a> {
    pub fn get_ast_node(&self, node_id: NodeId<AstNode>) -> &'a AstNode {
        self.ast.get(node_id)
    }
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum TypeError {
    #[error("operand(s) are invalid: {}", _0)]
    OpError(#[from] OperationError<Symbol>),

    #[error("invalid assignment: {}", _0)]
    AssignError(#[from] IncompatibleError<Symbol>),

    #[error("cannot assign to a value-type")]
    MissingDeclaredType,
}

trait HasType {
    fn get_type(
        &self,
        context: AssociatedTreeBuildContext<TypeInfo, AstNode>,
    ) -> (TypeInfo, Option<TypeError>);
}

impl HasType for howlite_syntax::ast::LiteralInteger {
    fn get_type(
        &self,
        _context: AssociatedTreeBuildContext<TypeInfo, AstNode>,
    ) -> (TypeInfo, Option<TypeError>) {
        (TypeInfo::new(Ty::Int(TyInt::single(self.value))), None)
    }
}

impl HasType for howlite_syntax::ast::ExprInfix {
    fn get_type<'a>(
        &self,
        context: AssociatedTreeBuildContext<TypeInfo, AstNode>,
    ) -> (TypeInfo, Option<TypeError>) {
        let lhs = context.get(self.lhs.clone());
        let rhs = context.get(self.rhs.clone());
        match self.op {
            howlite_syntax::ast::InfixOp::Add => {
                TypeInfo::synthesize_from([lhs, rhs], |[lhs, rhs]| {
                    lhs.arith_add(rhs).map_err(|e| (Ty::Hole, e.into()))
                })
            }
            howlite_syntax::ast::InfixOp::Sub => todo!(),
            howlite_syntax::ast::InfixOp::Div => todo!(),
            howlite_syntax::ast::InfixOp::Mul => {
                TypeInfo::synthesize_from([lhs, rhs], |[lhs, rhs]| {
                    lhs.arith_mul(rhs).map_err(|e| (Ty::Hole, e.into()))
                })
            }
            howlite_syntax::ast::InfixOp::Assign => {
                todo!()
                // TypeInfo::synthesize_from([lhs, rhs], |[lhs, rhs]| {
                //     lhs.is_assignable_to(rhs).map_err(|e| (Ty::Hole, e.into()))
                // })
            }
            howlite_syntax::ast::InfixOp::CmpNe => todo!(),
            howlite_syntax::ast::InfixOp::CmpEq => todo!(),
            howlite_syntax::ast::InfixOp::CmpGt => todo!(),
            howlite_syntax::ast::InfixOp::CmpLt => todo!(),
            howlite_syntax::ast::InfixOp::CmpGtEq => todo!(),
            howlite_syntax::ast::InfixOp::CmpLtEq => todo!(),
            howlite_syntax::ast::InfixOp::BitOr => todo!(),
            howlite_syntax::ast::InfixOp::BitAnd => todo!(),
            howlite_syntax::ast::InfixOp::BitXor => todo!(),
            howlite_syntax::ast::InfixOp::BitLShift => todo!(),
            howlite_syntax::ast::InfixOp::BitRShift => todo!(),
            howlite_syntax::ast::InfixOp::LogicalOr => todo!(),
            howlite_syntax::ast::InfixOp::LogicalAnd => todo!(),
        }
    }
}
