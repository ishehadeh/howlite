use hashbrown::HashMap;
use howlite_syntax::{tree::DefaultLinearTreeId, AstNodeData};

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol};

use super::{ModelBuilder, Term};

pub struct ConstraintTree<'b, 'c> {
    lexical_context: LexicalContext<'b, 'c>,
    pub constraint_terms: HashMap<DefaultLinearTreeId, Option<Term>>,
    pub model_builder: ModelBuilder,
    pub modified_vars: Vec<Symbol>,
}

impl<'b, 'c> ConstraintTree<'b, 'c> {
    pub fn new(root: LexicalContext<'b, 'c>) -> Self {
        Self {
            constraint_terms: Default::default(),
            lexical_context: root,
            model_builder: ModelBuilder::new(),
            modified_vars: Default::default(),
        }
    }

    fn gen_constraint_term(&mut self, node_id: DefaultLinearTreeId) -> Option<Term> {
        let child_node = self.lexical_context.get_node(node_id);
        match &child_node.data {
            AstNodeData::LiteralInteger(a) => Some(self.model_builder.add_lit_single(a.value)),
            AstNodeData::LiteralChar(a) => Some(self.model_builder.add_lit_single(a.value as i128)),
            AstNodeData::LiteralStructMember(_)
            | AstNodeData::LiteralArray(_)
            | AstNodeData::LiteralStruct(_)
            | AstNodeData::LiteralString(_) => None,

            AstNodeData::Ident(ident) => {
                let symbol = self.lexical_context.sym_intern(&ident.symbol);
                let ty = self.lexical_context.var_get_or_err(symbol).last_assignment;
                self.modified_vars.push(symbol);
                Some(
                    self.model_builder
                        .add_var(symbol, &ty.as_int().unwrap().values),
                )
            }

            // TODO: if we're accessing a variable here we can still infer things
            AstNodeData::ArrayAccess(_) | AstNodeData::FieldAccess(_) => None,

            AstNodeData::Repaired(_) => None,
            AstNodeData::DefFunc(_) => todo!(),
            AstNodeData::DefParam(_) => todo!(),
            AstNodeData::DefImport(_) => todo!(),

            // use blocks as a constant
            AstNodeData::Block(block) if block.returns && !block.statements.is_empty() => {
                self.gen_constraint_term(*block.statements.last().unwrap())
            }
            AstNodeData::Block(_) => None,

            AstNodeData::ExprIf(_) => todo!(),

            AstNodeData::ExprCall(_) => todo!(),
            AstNodeData::ExprInfix(infix) => {
                let op = infix.op;
                match self.get_constraint_terms([infix.lhs, infix.rhs]) {
                    [Some(lhs), Some(rhs)] => self.model_builder.do_infix(lhs, op, rhs),
                    _ => None,
                }
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

    pub fn get_constraint_term(&mut self, node_id: DefaultLinearTreeId) -> Option<Term> {
        // this function is a little odd to please the borrow checker.

        if self.constraint_terms.contains_key(&node_id) {
            return self.constraint_terms.get(&node_id).unwrap().clone();
        }
        let constraint = self.gen_constraint_term(node_id);
        self.constraint_terms.insert(node_id, constraint);
        self.constraint_terms.get(&node_id).unwrap().clone()
    }

    pub fn get_constraint_terms<const N: usize>(
        &mut self,
        node_ids: [DefaultLinearTreeId; N],
    ) -> [Option<Term>; N] {
        // close your eyes, this is going to be embarrasing.
        // what am I? A C programming?
        let mut nodes: [Option<Term>; N] = unsafe { std::mem::MaybeUninit::zeroed().assume_init() };

        for &node_id in node_ids.iter() {
            if !self.constraint_terms.contains_key(&node_id) {
                let constraint = self.gen_constraint_term(node_id);
                self.constraint_terms.insert(node_id, constraint);
            }
        }

        for (i, &node_id) in node_ids.iter().enumerate() {
            nodes[i] = self.constraint_terms.get(&node_id).unwrap().clone();
        }

        nodes
    }
}
