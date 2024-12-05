use hashbrown::HashMap;
use howlite_syntax::{ast::PrefixOp, tree::DefaultLinearTreeId, AstNodeData};
use howlite_typecheck::AccessPath;
use tracing::{debug, instrument};

use crate::{langctx::lexicalctx::LexicalContext, symtab::Symbol};

use super::{ModelBuilder, ModelVarRef, Term};

pub struct ConstraintTree<'b, 'c> {
    lexical_context: LexicalContext<'b, 'c>,
    pub constraint_terms: HashMap<DefaultLinearTreeId, Option<Term>>,
    pub model_builder: ModelBuilder,
    pub modified_vars: Vec<(ModelVarRef, Symbol, AccessPath<Symbol>)>,
    pub use_assumed_var_ty: bool,
}

impl<'b, 'c> ConstraintTree<'b, 'c> {
    pub fn new(root: LexicalContext<'b, 'c>, use_assumed_var_ty: bool) -> Self {
        Self {
            constraint_terms: Default::default(),
            lexical_context: root,
            model_builder: ModelBuilder::new(),
            modified_vars: Default::default(),
            use_assumed_var_ty,
        }
    }

    #[instrument(skip(self))]
    fn gen_constraint_term(&mut self, node_id: DefaultLinearTreeId) -> Option<Term> {
        let child_node = self.lexical_context.get_node(node_id);
        match &child_node.data {
            AstNodeData::LiteralInteger(a) => Some(self.model_builder.add_lit_single(a.value)),
            AstNodeData::LiteralChar(a) => Some(self.model_builder.add_lit_single(a.value as i128)),
            AstNodeData::LiteralStructMember(_)
            | AstNodeData::LiteralArray(_)
            | AstNodeData::LiteralStruct(_)
            | AstNodeData::LiteralString(_) => None,

            AstNodeData::Ident(_) | AstNodeData::ArrayAccess(_) | AstNodeData::FieldAccess(_) => {
                let mut top = node_id;
                let ty = self.lexical_context.child(top).synthesize_ty();
                debug!(?ty, ?node_id, "found possible narrow-able variable");
                ty.as_int()?;

                let mut path = AccessPath::default();
                loop {
                    debug!(?path, "building path");
                    match &self.lexical_context.get_node(top).data {
                        AstNodeData::Ident(ident) => {
                            let symbol = self.lexical_context.sym_intern(&ident.symbol);

                            let var = self.model_builder.hlt_var_id();
                            self.modified_vars
                                .push((var, symbol, path.clone()));
                            let hlt_var = self.lexical_context.var_get(symbol).unwrap();
                            let ty = if self.use_assumed_var_ty {
                                hlt_var.assumed_ty.access_path(path.as_slice())
                            } else {
                                hlt_var.last_assignment.access_path(path.as_slice())
                            }
                            .unwrap();
                            return Some(
                                self.model_builder
                                    .add_var(var, &ty.as_int().unwrap().values),
                            );
                        }
                        AstNodeData::ArrayAccess(_) => {
                            // can't infer array types
                            break;
                        }
                        AstNodeData::ExprPrefix(prefix) if prefix.op == PrefixOp::Deref => {
                            top = prefix.rhs;
                            path.push_deref();
                        }
                        AstNodeData::FieldAccess(field) => {
                            if field.field == "len"
                                && self
                                    .lexical_context
                                    .child(field.lhs)
                                    .synthesize_ty()
                                    .as_slice()
                                    .is_some()
                            {
                                break;
                            }
                            path.push_field(self.lexical_context.sym_intern(&field.field));
                            top = field.lhs
                        }
                        _ => break,
                    }
                }
                if let Some(int_ty) = ty.as_int() {
                    Some(self.model_builder.add_lit(&int_ty.values))
                } else {
                    None
                }
            }

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
            AstNodeData::ExprReturn(_) => todo!(),
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
