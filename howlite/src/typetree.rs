use howlite_syntax::{tree::NodeId, AstNode};
use howlite_typecheck::{
    errors::{IncompatibleError, OperationError},
    types::TyInt,
    Ty,
};
use sunstone::multi::DynSet;

use crate::symtab::{OwnedSymbolTable, Symbol, SymbolTable};

pub struct TypeTreeBuilder {
    symtab: OwnedSymbolTable,
}

impl TypeTreeBuilder {
    pub fn get_ast_node(&self, node_id: NodeId<AstNode>) -> AstNode {
        todo!("get_ast_node");
    }

    pub fn get_ty_node(&self, node_id: NodeId<AstNode>) -> Ty<Symbol> {
        todo!("get_ty_node")
    }

    pub fn set_ty_node(&self, node_id: NodeId<AstNode>, ty: Ty<Symbol>) {
        todo!("set_ty_node")
    }

    pub fn intern_symbol(&mut self, str: &str) -> Symbol {
        todo!("intern_symbol")
    }

    pub fn get_symbol(&self, symbol: Symbol) -> &str {
        todo!("get_symbol")
    }
}

#[derive(thiserror::Error, Debug)]
pub enum TypeError {
    #[error("operand(s) are invalid: {}", _0)]
    OpError(#[from] OperationError<Symbol>),

    #[error("invalid assignment: {}", _0)]
    AssignError(#[from] IncompatibleError<Symbol>),
}

trait HasType {
    fn get_type(&self, context: &mut TypeTreeBuilder) -> (Ty<Symbol>, Option<TypeError>);
}

impl HasType for howlite_syntax::ast::LiteralInteger {
    fn get_type(&self, _context: &mut TypeTreeBuilder) -> (Ty<Symbol>, Option<TypeError>) {
        (
            Ty::Int(TyInt {
                values: DynSet::new_from_individual(&[self.value.clone()]),
            }),
            None,
        )
    }
}

impl HasType for howlite_syntax::ast::ExprInfix {
    fn get_type(&self, context: &mut TypeTreeBuilder) -> (Ty<Symbol>, Option<TypeError>) {
        let lhs = context.get_ty_node(self.lhs.clone());
        let rhs = context.get_ty_node(self.rhs.clone());
        match self.op {
            howlite_syntax::ast::InfixOp::Add => match lhs.arith_add(&rhs) {
                Ok(t) => (t, None),
                Err(e) => (Ty::Hole, Some(e.into())),
            },
            howlite_syntax::ast::InfixOp::Sub => todo!(),
            howlite_syntax::ast::InfixOp::Div => todo!(),
            howlite_syntax::ast::InfixOp::Mul => match lhs.arith_mul(&rhs) {
                Ok(t) => (t, None),
                Err(e) => (Ty::Hole, Some(e.into())),
            },
            howlite_syntax::ast::InfixOp::Assign => match rhs.is_assignable_to(&lhs) {
                Ok(_) => (rhs, None),
                Err(e) => (lhs, Some(e.into())),
            },
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
