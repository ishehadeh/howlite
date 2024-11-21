use hashbrown::HashMap;
use howlite_syntax::{ast::InfixOp, tree::DefaultLinearTreeId, AstNodeData};
use preseli::IntegerSet;

use crate::langctx::lexicalctx::LexicalContext;

use super::{BinaryConstraintRelation, ConstraintOp, ConstraintTerm};

pub struct ConstraintTree<'a, 'b, 'c> {
    lexical_context: &'a LexicalContext<'b, 'c>,
    constraint_terms: HashMap<DefaultLinearTreeId, ConstraintTerm>,
}

impl<'a, 'b, 'c> ConstraintTree<'a, 'b, 'c> {
    pub fn new(root: &'a LexicalContext<'b, 'c>) -> Self {
        Self {
            constraint_terms: Default::default(),
            lexical_context: root,
        }
    }

    fn gen_constraint_term(&mut self, node_id: DefaultLinearTreeId) -> ConstraintTerm {
        let child_node = self.lexical_context.get_node(node_id);
        match &child_node.data {
            AstNodeData::LiteralInteger(a) => {
                ConstraintTerm::Literal(IntegerSet::new_from_range(a.value, a.value))
            }
            AstNodeData::LiteralChar(a) => ConstraintTerm::Literal(IntegerSet::new_from_range(
                a.value as i128,
                a.value as i128,
            )),
            AstNodeData::LiteralStructMember(_)
            | AstNodeData::LiteralArray(_)
            | AstNodeData::LiteralStruct(_)
            | AstNodeData::LiteralString(_) => ConstraintTerm::NotApplicable,

            AstNodeData::Ident(ident) => {
                let symbol = self.lexical_context.sym_intern(&ident.symbol);
                if let Some(var_def) = self.lexical_context.var_get(symbol) {
                    if var_def.is_mutable {
                        ConstraintTerm::Var(symbol)
                    } else if let Some(num) = var_def.last_assignment.as_int() {
                        ConstraintTerm::Literal(num.values.clone())
                    } else {
                        ConstraintTerm::NotApplicable
                    }
                } else {
                    ConstraintTerm::NotApplicable
                }
            }

            // TODO: if we're accessing a variable here we can still infer things
            AstNodeData::ArrayAccess(_) | AstNodeData::FieldAccess(_) => {
                ConstraintTerm::NotApplicable
            }

            AstNodeData::Repaired(_) => ConstraintTerm::NotApplicable,
            AstNodeData::DefFunc(_) => todo!(),
            AstNodeData::DefParam(_) => todo!(),
            AstNodeData::DefImport(_) => todo!(),

            AstNodeData::Block(block) if block.returns && !block.statements.is_empty() => {
                self.gen_constraint_term(*block.statements.last().unwrap())
            }
            AstNodeData::Block(_) => ConstraintTerm::NotApplicable,
            AstNodeData::ExprIf(_) => todo!(),

            AstNodeData::ExprCall(_) => todo!(),
            AstNodeData::ExprInfix(infix) => {
                let op = infix.op;
                let [lhs, rhs] = self.get_constraint_terms([infix.lhs, infix.rhs]);
                Self::apply_infix(lhs, op, rhs)
            }
            AstNodeData::ExprPrefix(_) => todo!(),
            AstNodeData::ExprTypeConstruction(_) => todo!(),
            AstNodeData::ExprLet(_) => todo!(),
            AstNodeData::ExprWhile(_) => todo!(),
            AstNodeData::DefType(_) => todo!(),
            AstNodeData::DefExternFunc(_) => todo!(),
            AstNodeData::DefExternVar(_) => todo!(),
            AstNodeData::Program(_) => todo!(),
            AstNodeData::TyRef(_) => todo!(),
            AstNodeData::TyExprUnion(_) => todo!(),
            AstNodeData::TyStruct(_) => todo!(),
            AstNodeData::TyStructMember(_) => todo!(),
            AstNodeData::TyNumberRange(_) => todo!(),
            AstNodeData::TyArray(_) => todo!(),
            AstNodeData::TyUnit(_) => todo!(),
            AstNodeData::TyParam(_) => todo!(),
            AstNodeData::TySlice(_) => todo!(),
            AstNodeData::TyNamed(_) => todo!(),
        }
    }

    pub fn get_constraint_term(&mut self, node_id: DefaultLinearTreeId) -> &ConstraintTerm {
        // this function is a little odd to please the borrow checker.

        if self.constraint_terms.contains_key(&node_id) {
            return self.constraint_terms.get(&node_id).unwrap();
        }
        let constraint = self.gen_constraint_term(node_id);
        self.constraint_terms.insert(node_id, constraint);
        self.constraint_terms.get(&node_id).unwrap()
    }

    pub fn get_constraint_terms<const N: usize>(
        &mut self,
        node_ids: [DefaultLinearTreeId; N],
    ) -> [&ConstraintTerm; N] {
        // close your eyes, this is going to be embarrasing.
        // what am I? A C programming?
        let mut nodes: [&ConstraintTerm; N] =
            unsafe { std::mem::MaybeUninit::zeroed().assume_init() };

        for &node_id in node_ids.iter() {
            if !self.constraint_terms.contains_key(&node_id) {
                let constraint = self.gen_constraint_term(node_id);
                self.constraint_terms.insert(node_id, constraint);
            }
        }

        for (i, &node_id) in node_ids.iter().enumerate() {
            nodes[i] = self.constraint_terms.get(&node_id).unwrap();
        }

        nodes
    }

    fn apply_infix(lhs: &ConstraintTerm, infix: InfixOp, rhs: &ConstraintTerm) -> ConstraintTerm {
        match infix {
            InfixOp::Add => lhs.apply_term(ConstraintOp::Add, rhs),
            InfixOp::Mul => lhs.apply_term(ConstraintOp::Mul, rhs),
            InfixOp::Sub => todo!("to_constraint_term: InfixOp::Sub"),
            InfixOp::Div => todo!("to_constraint_term: InfixOp::Div"),
            InfixOp::Assign => todo!(),
            InfixOp::CmpNe => lhs.compare_term(BinaryConstraintRelation::Ne, rhs),
            InfixOp::CmpEq => lhs.compare_term(BinaryConstraintRelation::Eq, rhs),
            InfixOp::CmpGt => lhs.compare_term(BinaryConstraintRelation::Gt, rhs),
            InfixOp::CmpLt => lhs.compare_term(BinaryConstraintRelation::Lt, rhs),
            InfixOp::CmpGtEq => todo!(">= constraint"),
            InfixOp::CmpLtEq => todo!("<= constraint"),
            InfixOp::BitOr => todo!("bit-wise constraints"),
            InfixOp::BitAnd => todo!("bit-wise constraints"),
            InfixOp::BitXor => todo!("bit-wise constraints"),
            InfixOp::BitLShift => todo!("bit-wise constraints"),
            InfixOp::BitRShift => todo!("bit-wise constraints"),
            InfixOp::LogicalOr => todo!("constraint groups"),
            InfixOp::LogicalAnd => todo!("constraint groups"),
        }
    }
}
