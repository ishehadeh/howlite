use std::collections::{BTreeSet, HashMap};

use crate::{
    parser::{ast, Ast},
    typecheck::{
        types::{ArrayType, RecordCell, RecordType, TyRef},
        FunctionDeclaration, ScalarType, TypeInfo,
    },
};

#[derive(Debug, Clone)]
pub struct TyParam {
    pub name: String,
    pub constraint: TypeInfo,
    pub default: Option<TypeInfo>,
}

#[derive(Debug, Clone)]
pub struct TyDecl {
    pub params: Vec<TyParam>,
    pub ty: TypeInfo,
}

#[derive(Debug, Clone, Default)]
pub struct Declarations {
    pub types: HashMap<String, TyDecl>,
    pub functions: HashMap<String, FunctionDeclaration>,
}

impl Declarations {
    pub fn from_ast<X: std::fmt::Debug + Clone>(program: &Ast<X>) -> Declarations {
        Declarations::from_ast_iter(std::iter::once(program))
    }

    pub fn from_ast_iter<'a, X: std::fmt::Debug + Clone + 'a>(
        program: impl Iterator<Item = &'a Ast<X>>,
    ) -> Declarations {
        let mut decls = Declarations::default();
        for toplevel_ast_node in program {
            match toplevel_ast_node {
                Ast::Program(p) => {
                    let child_decls = Declarations::from_ast_iter(p.definitions.iter());
                    decls.functions.extend(child_decls.functions.into_iter());
                    decls.types.extend(child_decls.types.into_iter());
                }
                Ast::DefFunction(func) => {
                    let paramaters = func
                        .params
                        .iter()
                        .map(|p| (p.name.clone(), decls.eval_anon_type(&p.typ)))
                        .collect();
                    let returns = decls.eval_anon_type(&func.return_ty);

                    decls.functions.insert(
                        func.name.clone(),
                        FunctionDeclaration {
                            paramaters,
                            returns,
                        },
                    );
                }

                Ast::DefExtern(extrm) => {
                    let paramaters = extrm
                        .params
                        .iter()
                        .map(|p| (p.name.clone(), decls.eval_anon_type(&p.typ)))
                        .collect();
                    let returns = decls.eval_anon_type(&extrm.return_ty);

                    decls.functions.insert(
                        extrm.name.clone(),
                        FunctionDeclaration {
                            paramaters,
                            returns,
                        },
                    );
                }

                Ast::DefType(t) => {
                    decls.types.insert(
                        t.name.clone(),
                        TyDecl {
                            params: t
                                .ty_params
                                .iter()
                                .map(|param| TyParam {
                                    name: param.name.clone(),
                                    constraint: decls.eval_anon_type(&param.super_ty),
                                    default: param
                                        .default_ty
                                        .as_ref()
                                        .map(|ty| decls.eval_anon_type(&ty)),
                                })
                                .collect(),
                            ty: decls.eval_anon_type(&t.ty),
                        },
                    );
                }
                // enumerate ignored items to make force future additions to the ast to be considered here before compiling
                Ast::LiteralInteger(_)
                | Ast::StmtWhile(_)
                | Ast::LiteralArray(_)
                | Ast::ArrayAccess(_)
                | Ast::LiteralBool(_)
                | Ast::Ident(_)
                | Ast::Repaired(_)
                | Ast::Block(_)
                | Ast::StmtIf(_)
                | Ast::ExprCall(_)
                | Ast::StmtLet(_)
                | Ast::FieldAccess(_)
                | Ast::StructLiteral(_)
                | Ast::Expr(_) => continue,
            }
        }

        decls
    }
    // TODO: remove this method an merge it into the typechecker,
    // this requires scanning ahead in the typechecker, for valid function and type names and definitions
    pub fn eval_anon_type<X: std::fmt::Debug + Clone>(&self, t: &ast::Ty<X>) -> TypeInfo {
        match t {
            ast::Ty::NumberRange(r) => TypeInfo::integer(
                r.inclusive_low.parse().unwrap(),
                r.inclusive_high.parse().unwrap(),
            ),
            ast::Ty::Unit(_) => TypeInfo::Unit,
            ast::Ty::TyRef(r) => {
                let parameters = r
                    .parameters
                    .iter()
                    .map(|p| self.eval_anon_type(p))
                    .collect();
                let name = r.name.to_string();
                TypeInfo::TyRef(TyRef { parameters, name })
            }
            ast::Ty::Bool(_) => TypeInfo::Scalar(ScalarType::Boolean(None)),
            ast::Ty::Struct(s) => {
                let mut fields = BTreeSet::new();
                let mut offset = 0;
                for m in &s.members {
                    let ty = self.eval_anon_type(&m.ty);
                    let ty_size = ty.get_size();
                    fields.insert(RecordCell {
                        name: m.name.clone(),
                        offset,
                        length: ty_size,
                        type_info: ty,
                    });
                    offset += ty_size
                }
                TypeInfo::Record(RecordType { fields })
            }
            ast::Ty::Array(a) => TypeInfo::Array(ArrayType {
                length: a.length,
                element_ty: Box::new(self.eval_anon_type(&a.element_ty)),
            }),
        }
    }
}
