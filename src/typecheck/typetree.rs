use std::collections::{BTreeSet, HashMap};

use crate::{
    parser::{
        ast::{self, InfixOp, Repaired, SyntaxData, SyntaxNodeRef, SyntaxTree, XData},
        Ast,
    },
    util::ast::{Declarations, TyDecl},
};

use super::{
    types::{ArrayType, TyRef},
    RecordCell, RecordType, ScalarType, TypeError, TypeInfo,
};

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Debug)]
pub struct TypeTreeXData {
    pub declared_type: TypeInfo,
    pub value_type: Option<TypeInfo>,
    pub error: Option<TypeError>,
    pub cond_true: HashMap<String, TypeInfo>,
    pub cond_false: HashMap<String, TypeInfo>,
}

impl TypeTreeXData {
    pub fn new(declared_type: TypeInfo) -> TypeTreeXData {
        TypeTreeXData {
            declared_type,
            value_type: None,
            error: None,
            cond_false: Default::default(),
            cond_true: Default::default(),
        }
    }

    // get the current value type or declared type if there is no value type
    pub fn current_type(&self) -> &TypeInfo {
        match &self.value_type {
            Some(a) => a,
            None => &self.declared_type,
        }
    }

    pub fn new_value(value: TypeInfo) -> TypeTreeXData {
        TypeTreeXData {
            declared_type: value.clone(),
            value_type: Some(value),
            error: None,
            cond_false: Default::default(),
            cond_true: Default::default(),
        }
    }

    pub fn new_error(declared_type: TypeInfo, error: TypeError) -> TypeTreeXData {
        TypeTreeXData {
            declared_type,
            value_type: None,
            error: Some(error),
            cond_false: Default::default(),
            cond_true: Default::default(),
        }
    }
}

impl Default for TypeTreeXData {
    fn default() -> TypeTreeXData {
        TypeTreeXData::new(TypeInfo::Unit)
    }
}

pub type TypeTree = Ast<TypeTreeXData>;

pub struct TyContext {}

pub trait GetTyData {
    fn get_ty_data(
        &self,
        tree: &SyntaxTree,
        ty_database: &SyntaxData<TypeTreeXData>,
        ctx: &mut TyContext,
    ) -> TypeTreeXData;
}

pub trait GetTyDataPure {
    fn get_ty_data_pure(&self) -> TypeTreeXData;
}

impl<T: GetTyDataPure> GetTyData for T {
    fn get_ty_data(
        &self,
        _tree: &SyntaxTree,
        _ty_database: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        self.get_ty_data_pure()
    }
}

#[derive(Default, Clone, Debug)]
pub struct TypeInterpreter {
    declarations: Declarations,
    scopes: Vec<TypeScope>,
}

#[derive(Default, Clone, Debug)]

pub struct TypeScope {
    pub variables: HashMap<String, TypeTreeXData>,
    pub tys: HashMap<String, TyDecl>,
}

impl GetTyDataPure for ast::LiteralInteger {
    fn get_ty_data_pure(&self) -> TypeTreeXData {
        TypeTreeXData::new(TypeInfo::integer(self.value, self.value))
    }
}

impl GetTyDataPure for ast::LiteralBool {
    fn get_ty_data_pure(&self) -> TypeTreeXData {
        TypeTreeXData::new(TypeInfo::bool())
    }
}

impl TypeInterpreter {
    pub fn new(declarations: Declarations) -> TypeInterpreter {
        TypeInterpreter {
            scopes: vec![Default::default()],
            declarations,
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(TypeScope::default())
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("no scope to pop");
    }

    // retrieve a copy of a variable's type data, or return an error and with declt type Unit if no such variable exists
    pub fn get_var(&self, name: &str) -> TypeTreeXData {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.variables.get(name) {
                return v.clone();
            }
        }

        TypeTreeXData::new_error(
            TypeInfo::Unit,
            TypeError::UnknownVariable {
                name: name.to_string(),
            },
        )
    }

    pub fn set_var(&mut self, name: &str, data: TypeTreeXData) {
        let top = self
            .scopes
            .last_mut()
            .expect("no scopes! (should be unreachable)");
        top.variables.insert(name.to_string(), data);
    }

    pub fn get_ty(&self, name: &str) -> Option<&TyDecl> {
        self.scopes.iter().rev().find_map(|s| s.tys.get(name))
    }

    pub fn set_ty(&mut self, name: &str, data: TyDecl) {
        let top = self
            .scopes
            .last_mut()
            .expect("no scopes! (should be unreachable)");
        top.tys.insert(name.to_string(), data);
    }

    pub fn eval_ast(&mut self, ast: Ast) -> TypeTree {
        match ast {
            Ast::LiteralInteger(i) => Ast::LiteralInteger(TypeInterpreter::eval_literal_integer(i)),
            Ast::LiteralBool(b) => Ast::LiteralBool(TypeInterpreter::eval_literal_bool(b)),
            Ast::Ident(i) => Ast::Ident(ast::Ident {
                span: i.span,
                xdata: self.get_var(&i.symbol),
                symbol: i.symbol,
            }),
            Ast::Repaired(r) => Ast::Repaired(Repaired {
                xdata: Default::default(),
                span: r.span,
                tree: r.tree.map(|t| Box::new(self.eval_ast(*t))),
            }),
            Ast::DefFunction(f) => Ast::DefFunction(self.eval_def_function(f)),
            Ast::Block(b) => Ast::Block(self.eval_block(b)),
            Ast::StmtIf(i) => Ast::StmtIf(self.eval_stmt_if(i)),
            Ast::ExprCall(c) => Ast::ExprCall(self.eval_expr_call(c)),
            Ast::Expr(e) => Ast::Expr(self.eval_expr(e)),
            Ast::StmtLet(l) => Ast::StmtLet(self.eval_stmt_let(l)),
            Ast::DefType(t) => {
                let ty_params = t
                    .ty_params
                    .into_iter()
                    .map(|x| self.eval_ty_param(x))
                    .collect();
                let ty_node = self.eval_ty(t.ty);
                // handled when scanning decls
                Ast::DefType(ast::DefType {
                    span: t.span,
                    xdata: Default::default(),
                    name: t.name,
                    ty: ty_node,
                    ty_params,
                })
            }
            Ast::Program(p) => Ast::Program(self.eval_program(p)),
            Ast::FieldAccess(f) => Ast::FieldAccess(self.eval_field_access(f)),
            Ast::StructLiteral(s) => Ast::StructLiteral(self.eval_struct_literal(s)),
            Ast::LiteralArray(a) => Ast::LiteralArray(self.eval_literal_array(a)),
            Ast::ArrayAccess(a) => Ast::ArrayAccess(self.eval_array_access(a)),
            Ast::StmtWhile(w) => Ast::StmtWhile(self.eval_while(w)),
            Ast::DefExtern(e) => Ast::DefExtern(self.eval_def_extern(e)),
        }
    }

    pub fn eval_def_function(&mut self, f: ast::DefFunction) -> ast::DefFunction<TypeTreeXData> {
        self.push_scope();
        let mut params: Vec<_> = Vec::new();
        for p in f.params {
            let ty_param = self.eval_param(p);
            self.set_var(&ty_param.name, ty_param.xdata().clone());
            params.push(ty_param);
        }

        let body_type_tree = self.eval_ast(*f.body);
        let return_ty_node = self.eval_ty(f.return_ty);
        let return_ty = return_ty_node.xdata().current_type();
        self.pop_scope();
        let error = if !body_type_tree.xdata().current_type().is_subset(return_ty) {
            Some(TypeError::BadFunctionReturnType {
                returned: body_type_tree.xdata().current_type().clone(),
                expected: return_ty.clone(),
            })
        } else {
            None
        };
        ast::DefFunction {
            span: f.span,
            xdata: TypeTreeXData {
                declared_type: return_ty.clone(),
                error,
                value_type: None,
                cond_false: Default::default(),
                cond_true: Default::default(),
            },
            name: f.name,
            params,
            return_ty: return_ty_node,
            body: Box::new(body_type_tree),
        }
    }

    pub fn eval_block(&mut self, b: ast::Block) -> ast::Block<TypeTreeXData> {
        self.push_scope();

        let statements: Vec<_> = b.statements.into_iter().map(|s| self.eval_ast(s)).collect();

        let xdata = if b.returns {
            statements
                .last()
                .map(|s| s.xdata().clone())
                .unwrap_or(Default::default())
        } else {
            Default::default()
        };

        self.pop_scope();
        ast::Block {
            span: b.span,
            xdata,
            returns: b.returns,
            statements,
        }
    }

    pub fn eval_literal_array(&mut self, a: ast::LiteralArray) -> ast::LiteralArray<TypeTreeXData> {
        self.push_scope();

        let values: Vec<_> = a.values.into_iter().map(|s| self.eval_ast(s)).collect();
        let element_ty = TypeInfo::union(
            values
                .iter()
                .map(|v| v.xdata().current_type())
                .cloned()
                .collect::<Vec<_>>(),
        );
        self.pop_scope();

        ast::LiteralArray {
            span: a.span,
            xdata: TypeTreeXData::new(TypeInfo::Array(ArrayType {
                length: values.len() as u32,
                element_ty: Box::new(element_ty),
            })),
            values,
        }
    }

    pub fn apply_infix_op_on_type(
        op: InfixOp,
        lhs: &TypeInfo,
        rhs: &TypeInfo,
    ) -> Result<TypeInfo, TypeError> {
        match (op, lhs, rhs) {
            (
                InfixOp::CmpEq
                | InfixOp::CmpNe
                | InfixOp::CmpLe
                | InfixOp::CmpGe
                | InfixOp::CmpGt
                | InfixOp::CmpLt,
                TypeInfo::Scalar(_),
                TypeInfo::Scalar(_),
            ) => Ok(TypeInfo::bool()),

            (
                InfixOp::Add | InfixOp::Sub | InfixOp::Div | InfixOp::Mul,
                TypeInfo::Scalar(ScalarType::Integer(a)),
                TypeInfo::Scalar(ScalarType::Integer(b)),
            ) => Ok(match op {
                InfixOp::Add => TypeInfo::integer(a.lo + b.lo, a.hi + b.hi),
                InfixOp::Sub => TypeInfo::integer(a.lo - b.hi, a.hi - b.lo),
                InfixOp::Div => TypeInfo::integer(a.lo / b.hi, a.hi / b.lo),
                InfixOp::Mul => TypeInfo::integer(a.lo * b.lo, a.hi * b.hi),
                _ => unreachable!(),
            }),

            (InfixOp::Assign, lhs, rhs) => {
                if !rhs.is_subset(rhs) {
                    Err(TypeError::BadAssignment {
                        binding_type: lhs.clone(),
                        value_type: rhs.clone(),
                        binding_name: String::new(),
                    })
                } else {
                    Ok(rhs.clone())
                }
            }

            _ => Err(TypeError::BadExpression {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                op,
            }),
        }
    }

    pub fn type_from_conditional(
        &mut self,
        op: InfixOp,
        lhs: &ast::Ast<TypeTreeXData>,
        rhs: &ast::Ast<TypeTreeXData>,
    ) -> (HashMap<String, TypeInfo>, HashMap<String, TypeInfo>) {
        let (ident, expr, is_expr_rhs) = match (lhs, rhs) {
            (lhs, TypeTree::Ident(ident)) => (ident, lhs, false),
            (TypeTree::Ident(ident), rhs) => (ident, rhs, true),
            _ => return Default::default(),
        };

        let expr_ty = expr.xdata().current_type();
        let ident_ty = ident.xdata().current_type();

        let var_name = match op {
            InfixOp::Mul | InfixOp::Div | InfixOp::Add | InfixOp::Sub => return Default::default(),
            InfixOp::CmpEq => (expr_ty.clone(), ident_ty.clone(), false),
            InfixOp::CmpNe => (ident_ty.clone(), expr_ty.clone(), false),
            InfixOp::CmpLt => {
                if let TypeInfo::Scalar(ScalarType::Integer(x)) = expr_ty {
                    let lt_ty = TypeInfo::integer(i32::MIN, x.hi - 1);
                    let ge_ty = TypeInfo::integer(x.lo, i32::MAX);
                    (ident_ty.intersect(&lt_ty), ident_ty.intersect(&ge_ty), true)
                } else {
                    return Default::default();
                }
            }
            InfixOp::CmpGt => {
                if let TypeInfo::Scalar(ScalarType::Integer(x)) = expr_ty {
                    let le_ty = TypeInfo::integer(i32::MIN, x.hi);
                    let gt_ty = TypeInfo::integer(x.lo + 1, i32::MAX);
                    (ident_ty.intersect(&gt_ty), ident_ty.intersect(&le_ty), true)
                } else {
                    return Default::default();
                }
            }
            InfixOp::CmpGe => {
                if let TypeInfo::Scalar(ScalarType::Integer(x)) = expr_ty {
                    let lt_ty = TypeInfo::integer(i32::MIN, x.hi - 1);
                    let ge_ty = TypeInfo::integer(x.lo, i32::MAX);
                    (ident_ty.intersect(&ge_ty), ident_ty.intersect(&lt_ty), true)
                } else {
                    return Default::default();
                }
            }
            InfixOp::CmpLe => {
                if let TypeInfo::Scalar(ScalarType::Integer(x)) = expr_ty {
                    let le_ty = TypeInfo::integer(i32::MIN, x.hi);
                    let gt_ty = TypeInfo::integer(x.lo + 1, i32::MAX);
                    (ident_ty.intersect(&le_ty), ident_ty.intersect(&gt_ty), true)
                } else {
                    return Default::default();
                }
            }
            InfixOp::Assign => return Default::default(),
        };
        let (cond_true, cond_false, should_flip) = var_name;

        // output of the above match assumes the variable is on the left and expr is on the right
        // if that isn't the case (is_expr_rhs == false) and the result is inverted if the operatorands are switched
        // (should_flip) then flip invert the conditions
        let (cond_true, cond_false) = if should_flip && is_expr_rhs {
            (cond_true, cond_false)
        } else {
            (cond_false, cond_true)
        };
        (
            HashMap::from([(ident.symbol.clone(), cond_true)]),
            HashMap::from([(ident.symbol.clone(), cond_false)]),
        )
    }

    pub fn eval_expr(&mut self, expr: ast::Expr) -> ast::Expr<TypeTreeXData> {
        let lhs = self.eval_ast(*expr.lhs);
        let rhs = self.eval_ast(*expr.rhs);
        let lhs_ty = lhs.xdata();
        let rhs_ty = lhs.xdata();

        let val_ty_tuple = match (&lhs_ty.value_type, &rhs_ty.value_type) {
            (Some(a), Some(b)) => Some((a, b)),
            _ => None,
        };
        let decl =
            Self::apply_infix_op_on_type(expr.op, lhs_ty.current_type(), &rhs_ty.current_type());
        let value_type = val_ty_tuple
            .and_then(|(lhs, rhs)| Self::apply_infix_op_on_type(expr.op, lhs, rhs).ok());

        let (cond_true, cond_false) = self.type_from_conditional(expr.op, &lhs, &rhs);

        ast::Expr {
            span: expr.span,
            xdata: TypeTreeXData {
                error: decl.clone().err(),
                declared_type: decl.unwrap_or(TypeInfo::Unit),
                value_type,
                cond_false,
                cond_true,
            },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: expr.op,
        }
    }

    fn eval_stmt_let(&mut self, l: ast::StmtLet) -> ast::StmtLet<TypeTreeXData> {
        let ty = self.eval_ty(l.ty);
        let binding_type = ty.xdata().current_type();
        let typed_value_expr = self.eval_ast(*l.value);
        let value_type = typed_value_expr.xdata().current_type().clone();
        let error = if !value_type.is_subset(&binding_type) {
            Some(TypeError::BadAssignment {
                binding_type: binding_type.clone(),
                value_type,
                binding_name: l.name.clone(),
            })
        } else {
            None
        };
        let xdata = TypeTreeXData {
            declared_type: binding_type.clone(),
            value_type: Some(typed_value_expr.xdata().current_type().clone()),
            error,
            cond_false: Default::default(),
            cond_true: Default::default(),
        };
        self.set_var(&l.name, xdata.clone());
        ast::StmtLet {
            span: l.span,
            xdata,
            name: l.name,
            mutable: l.mutable,
            ty,
            value: Box::new(typed_value_expr),
        }
    }

    fn eval_stmt_if(&mut self, i: ast::StmtIf) -> ast::StmtIf<TypeTreeXData> {
        let typed_cond = self.eval_ast(*i.condition);
        let error = if !typed_cond
            .xdata()
            .current_type()
            .is_subset(&TypeInfo::bool())
        {
            Some(TypeError::ExpectedCondition {
                recieved: typed_cond.xdata().current_type().clone(),
            })
        } else {
            None
        };

        // eval body
        self.push_scope();
        for (var, value_type) in typed_cond.xdata().cond_true.iter() {
            let mut xdata = self.get_var(&var);
            xdata.value_type = Some(value_type.clone());
            self.set_var(var, xdata)
        }
        let body = self.eval_ast(*i.body);
        self.pop_scope();

        // eval else
        let else_ = if let Some(old_else) = i.else_ {
            self.push_scope();
            for (var, value_type) in typed_cond.xdata().cond_false.iter() {
                let mut xdata = self.get_var(&var);
                xdata.value_type = Some(value_type.clone());
                self.set_var(var, xdata)
            }
            let else_ = self.eval_ast(*old_else);
            self.pop_scope();
            Some(Box::new(else_))
        } else {
            None
        };

        let typ: TypeInfo = if let Some(else_) = &else_ {
            TypeInfo::union([
                else_.xdata().current_type().clone(),
                body.xdata().current_type().clone(),
            ])
        } else {
            body.xdata().current_type().clone()
        };

        ast::StmtIf {
            span: i.span,
            xdata: TypeTreeXData {
                declared_type: typ.clone(),
                value_type: Some(typ),
                error,
                cond_true: Default::default(),
                cond_false: Default::default(),
            },
            condition: Box::new(typed_cond),
            body: Box::new(body),
            else_,
        }
    }

    fn eval_expr_call(&mut self, c: ast::ExprCall) -> ast::ExprCall<TypeTreeXData> {
        let typed_parameters = c.paramaters.into_iter().map(|p| self.eval_ast(p)).collect();
        let decl = match self.declarations.functions.get(&c.function_name) {
            Some(d) => d,
            None => {
                return ast::ExprCall {
                    xdata: TypeTreeXData::new_error(
                        TypeInfo::Unit,
                        TypeError::UnknownFunction {
                            name: c.function_name.clone(),
                        },
                    ),
                    span: c.span,
                    function_name: c.function_name,
                    paramaters: typed_parameters,
                }
            }
        };

        // TODO: type check params
        ast::ExprCall {
            xdata: TypeTreeXData::new(self.resolve_ty_refs(decl.returns.clone(), &[])),
            span: c.span,
            function_name: c.function_name,
            paramaters: typed_parameters,
        }
    }

    pub fn resolve_ty_refs(&self, t: TypeInfo, parameters: &[(String, TypeInfo)]) -> TypeInfo {
        match t {
            TypeInfo::Unit | TypeInfo::Scalar(_) => t.clone(),
            TypeInfo::Union(u) => TypeInfo::union(
                u.types
                    .into_iter()
                    .map(|f| self.resolve_ty_refs(f, parameters))
                    .collect::<Vec<TypeInfo>>(),
            ),
            TypeInfo::Record(r) => TypeInfo::Record(RecordType {
                fields: r
                    .fields
                    .into_iter()
                    .map(|f| RecordCell {
                        length: f.length,
                        offset: f.offset,
                        name: f.name,
                        type_info: self.resolve_ty_refs(f.type_info, parameters),
                    })
                    .collect(),
            }),
            TypeInfo::TyRef(ty_ref) => {
                let param_ty = parameters.iter().find(|(n, _)| *n == ty_ref.name);
                if let Some((_, ty)) = param_ty {
                    ty.clone()
                } else {
                    let ty_decl = self
                        .declarations
                        .types
                        .get(&ty_ref.name)
                        .expect("cannot find type!"); // TODO: real error handling
                    let mut params_given = ty_ref.parameters.iter();
                    let mut params = Vec::with_capacity(ty_decl.params.len());
                    for param_decl in ty_decl.params.iter() {
                        let ty = if let Some(given) = params_given.next() {
                            given.clone()
                        } else {
                            param_decl.default.clone().expect("bad type param")
                            // TODO real error
                        };
                        params.push((param_decl.name.clone(), ty));
                    }
                    self.resolve_ty_refs(ty_decl.ty.clone(), &params)
                }
            }
            TypeInfo::Array(a) => TypeInfo::Array(ArrayType {
                element_ty: Box::new(self.resolve_ty_refs(*a.element_ty, &[])),
                length: a.length,
            }),
        }
    }

    fn eval_ty_param(&mut self, x: ast::TyParam) -> ast::TyParam<TypeTreeXData> {
        let super_ty = self.eval_ty(x.super_ty);
        ast::TyParam {
            span: x.span,
            xdata: TypeTreeXData::new(super_ty.xdata().current_type().clone()),
            name: x.name,
            super_ty,
            default_ty: x.default_ty,
        }
    }

    fn eval_array_access(&mut self, a: ast::ArrayAccess) -> ast::ArrayAccess<TypeTreeXData> {
        let object = self.eval_ast(*a.object);
        let index = self.eval_ast(*a.index);
        let xdata = match object.xdata().current_type() {
            TypeInfo::Array(a) => {
                if index
                    .xdata()
                    .current_type()
                    .is_subset(&TypeInfo::integer(0, a.length as i32))
                {
                    TypeTreeXData::new(*a.element_ty.clone())
                } else {
                    TypeTreeXData::new_error(
                        *a.element_ty.clone(),
                        TypeError::InvalidArrayAccess {
                            object: object.xdata().current_type().clone(),
                            index: index.xdata().current_type().clone(),
                        },
                    )
                }
            }

            object_ty => TypeTreeXData::new_error(
                TypeInfo::Unit,
                TypeError::ArrayAccessOnNonArray {
                    object: object_ty.clone(),
                },
            ),
        };

        ast::ArrayAccess {
            object: Box::new(object),
            index: Box::new(index),
            span: a.span,
            xdata,
        }
    }

    fn eval_while(&mut self, w: ast::StmtWhile) -> ast::StmtWhile<TypeTreeXData> {
        let condition = self.eval_ast(*w.condition);
        let xdata = if !condition
            .xdata()
            .current_type()
            .is_subset(&TypeInfo::bool())
        {
            TypeTreeXData::new_error(
                TypeInfo::Unit,
                TypeError::ExpectedCondition {
                    recieved: condition.xdata().current_type().clone(),
                },
            )
        } else {
            TypeTreeXData::new(TypeInfo::Unit)
        };
        let body = self.eval_ast(*w.body);
        ast::StmtWhile {
            xdata,
            span: w.span,
            condition: Box::new(condition),
            body: Box::new(body),
        }
    }
}
