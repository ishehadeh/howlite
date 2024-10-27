use std::{
    borrow::Borrow,
    collections::{vec_deque, VecDeque},
    rc::Rc,
    sync::{
        atomic::{AtomicU64, Ordering},
        RwLock,
    },
};

use dashmap::DashMap;
use howlite_syntax::{
    ast::{
        BoxAstNode, ExprInfix, ExprLet, HigherOrderNode, InfixOp, LiteralArray, LiteralChar,
        LiteralInteger, LiteralString, LiteralStruct,
    },
    AstNode, AstNodeData, Span,
};
use howlite_typecheck::{
    errors::OperationError,
    types::TyUnion,
    types::{self, StorageClass, TyInt},
    Ty, TyArray,
};
use preseli::IntegerSet;
use smallvec::SmallVec;
use thiserror::Error;

use crate::symtab::{Symbol, SyncSymbolTable};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(u64);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorId(u64);

#[derive(Clone, Debug)]
pub struct CompilationError<SourceLocationT> {
    pub location: SourceLocationT,
    pub kind: CompilationErrorKind,
}

#[derive(Error, Debug, Clone)]
pub enum CompilationErrorKind {
    #[error("invalid arithmetic operation: {}", _0)]
    InvalidArithmetic(#[from] OperationError<Symbol>),

    #[error("Type {}: expcted {} type parameters, got {}", ty, expected, got)]
    IncorrectTyParamCount {
        expected: usize,
        got: usize,
        ty: Symbol,
    },
}

pub struct LangCtx<SourceLocationT> {
    pub symbols: SyncSymbolTable,
    scopes: DashMap<ScopeId, Scope>,
    errors: DashMap<ErrorId, CompilationError<SourceLocationT>>,
    scope_parent: RwLock<Vec<(ScopeId, ScopeId)>>,
    root_scope_id: ScopeId,
}

/* #region impl LangCtx: ID Helpers */
impl<L> LangCtx<L> {
    /// create a new ScopeId, these identifiers are globally unique within the process
    fn mint_scope_id() -> ScopeId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ScopeId(NEXT.fetch_add(1, Ordering::Relaxed))
    }

    /// create a new ErrorId, these identifiers are globally unique within the process
    fn mint_error_id() -> ErrorId {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        ErrorId(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}
/* #endregion */

/* #region impl LangCtx: General Functionality */
impl<L> LangCtx<L> {
    pub fn new() -> Self {
        let scopes = DashMap::new();
        let root_scope_id = Self::mint_scope_id();
        scopes.insert(root_scope_id, Scope::default());

        Self {
            symbols: Default::default(),
            scope_parent: Default::default(),
            errors: Default::default(),
            scopes,
            root_scope_id,
        }
    }

    /// Get the parent scope, return None if `scope` is the root scope.
    ///
    /// # Panics
    /// This function panics if `scope` does not belong to this `LangCtx``
    pub fn scope_parent(&self, child: ScopeId) -> Option<ScopeId> {
        if child == self.root_scope_id {
            None
        } else {
            let (child_from_vec, parent) = {
                let scope_parent = self
                    .scope_parent
                    .read()
                    .expect("LangCtx::scope_parent(): RwLock is poisoned!");
                debug_assert!(
                    scope_parent.is_sorted(),
                    "LangCtx::scope_parent(): parent-child map isn't sorted, but we depend on binary search to find parents."
                );
                let pair_index = scope_parent
                .binary_search_by_key(&child, |&(child, _parent)| child)
                .expect("LangCtx::scope_parent(): could not find a parent for scope. There is either a bug in LangCtx's scope-create logic, or this scope does not belong to this instance of LangCtx");
                scope_parent[pair_index]
            };
            debug_assert_eq!(child, child_from_vec, "LangCtx::scope_parent(): binary search found the wrong value, this should be impossible");
            Some(parent)
        }
    }

    pub fn scope_new(&self, parent: ScopeId) -> ScopeId {
        let new_scope_id = Self::mint_scope_id();
        {
            let mut scope_parent = self
                .scope_parent
                .write()
                .expect("LangCtx::scope_new(): RwLock is poisoned!");
            debug_assert!(
                scope_parent.is_sorted(),
                "LangCtx::scope_new(): parent-child map isn't sorted, but we depend on binary search to find parents."
            );
            scope_parent.push((new_scope_id, parent));
            debug_assert!(
                scope_parent.is_sorted(),
                "LangCtx::scope_new(): new scope IDs should always be increasing, but inserting at the end of the array did not maintain sort order"
            );
        }

        debug_assert!(
            self.scopes.insert(new_scope_id, Scope::default()).is_some(),
            "LangCtx::scope_new(): scope exists, this should be impossible"
        );

        new_scope_id
    }

    /// Recursively search for a variable up the scope heirarchy
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    /// - Any panics associated with `self.scope_parent()`
    pub fn var_get(&self, scope_id: ScopeId, var: Symbol) -> Option<VarDef> {
        if let Some(scope) = self.scopes.get(&scope_id) {
            if let Some(def) = scope.get_local(var) {
                Some(def.clone())
            } else if let Some(parent) = self.scope_parent(scope_id) {
                self.var_get(parent, var)
            } else {
                // we're at the root scope
                None
            }
        } else {
            panic!(
                "LangCtx::VarGet(): Scope is not associated with this LangCtx (scope={:?})",
                scope_id
            )
        }
    }

    /// Define a variable in the given scope
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    pub fn var_def(&self, scope_id: ScopeId, var: Symbol, def: VarDef) {
        if let Some(mut scope) = self.scopes.get_mut(&scope_id) {
            scope.locals.push((var, def));
        } else {
            panic!(
                "LangCtx::VarGet(): Scope is not associated with this LangCtx (scope={:?})",
                scope_id
            )
        }
    }

    /// Recursively search for a type up the scope heirarchy
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    /// - Any panics associated with `self.scope_parent()`
    pub fn ty_get(&self, scope_id: ScopeId, ty: Symbol) -> Option<TyDef> {
        if let Some(scope) = self.scopes.get(&scope_id) {
            if let Some(def) = scope.get_ty(ty) {
                Some(def.clone())
            } else if let Some(parent) = self.scope_parent(scope_id) {
                self.ty_get(parent, ty)
            } else {
                // we're at the root scope
                None
            }
        } else {
            panic!(
                "LangCtx::VarGet(): Scope is not associated with this LangCtx (scope={:?})",
                scope_id
            )
        }
    }

    /// Define a named type in the given scope
    ///
    /// # Panics
    /// - Scope does not belong to this LangCtx
    pub fn ty_def(&self, scope_id: ScopeId, def: TyDef) {
        if let Some(mut scope) = self.scopes.get_mut(&scope_id) {
            scope.tys.push(def);
        } else {
            panic!(
                "LangCtx::ty_def(): Scope is not associated with this LangCtx (scope={:?})",
                scope_id
            )
        }
    }

    /// Define a variable in the given scope
    pub fn error(&self, err: CompilationError<L>) -> ErrorId {
        let id = Self::mint_error_id();
        debug_assert!(
            self.errors.insert(id, err).is_some(),
            "LangCtx::error(): error exists, this should be impossible"
        );
        id
    }
}
/* #endregion */

impl<L> Default for LangCtx<L> {
    fn default() -> Self {
        Self::new()
    }
}
#[derive(Debug, Default)]
pub struct Scope {
    /// List of local variable definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    locals: Vec<(Symbol, VarDef)>,

    /// List of local variable definitions.
    /// There can be duplicate symbols if a symbol is redefined
    /// the list MUST be in the order variables are defined
    tys: Vec<TyDef>,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub assumed_ty: Rc<Ty<Symbol>>,
    pub last_assignment: Rc<Ty<Symbol>>,
}

#[derive(Debug, Clone)]
pub struct TyDef {
    pub name: Symbol,
    pub params: Vec<(Symbol, Rc<Ty<Symbol>>)>,
    pub ty: Rc<Ty<Symbol>>,
}

impl TyDef {
    pub fn instantiate<SourceLocationT>(
        &self,
        err_location: SourceLocationT,
        ctx: &LangCtx<SourceLocationT>,
        params: &[Rc<Ty<Symbol>>],
    ) -> Rc<Ty<Symbol>> {
        if params.len() != self.params.len() {
            ctx.error(CompilationError {
                location: err_location,
                kind: CompilationErrorKind::IncorrectTyParamCount {
                    expected: self.params.len(),
                    got: params.len(),
                    ty: self.name,
                },
            });
        };

        let get_ty_param = |param: Symbol| {
            self.params
                .iter()
                .enumerate()
                .find_map(|(i, &(sym, _))| if sym == param { Some(i) } else { None })
                .map(|i| {
                    params
                        .iter()
                        .nth(i)
                        .cloned()
                        .unwrap_or_else(|| Rc::new(Ty::Hole))
                })
        };

        let mut ty_stack: SmallVec<[(usize, Rc<Ty<Symbol>>); 4]> = SmallVec::new_const();

        ty_stack.push((usize::MAX, self.ty));
        let mut last_ty: _;
        while let Some((state, ty)) = ty_stack.last().cloned() {
            match ty {
                &Ty::Hole => (),
                Ty::Int(_) => (),
                Ty::Struct(struc) => {
                    for (i, field) in struc.fields.iter().enumerate() {
                        if !matches!(*field.ty, Ty::Hole | Ty::Int(_)) {
                            ty_stack.last_mut().unwrap().0 = i;
                            ty_stack.push((usize::MAX, field.ty.clone()));
                        }
                    }
                }
                &Ty::Array(arr) => {
                    if state == 1 {
                        arr.element_ty = last_ty;
                    } else {
                        ty_stack.last_mut().unwrap().0 = 1;
                        ty_stack.push((usize::MAX, arr.element_ty.clone()));
                        break;
                    }
                }
                Ty::Slice(slice) => {
                    if state == 1 {
                        slice.element_ty = last_ty;
                        ty_stack.last_mut().unwrap().0 = 2;
                        ty_stack.push((usize::MAX, slice.index_set.clone()));
                    } else if state == 2 {
                        slice.index_set = last_ty;
                    } else if !matches!(*slice.element_ty, Ty::Hole | Ty::Int(_)) {
                        ty_stack.last_mut().unwrap().0 = 1;
                        ty_stack.push((usize::MAX, slice.element_ty.clone()));
                        break;
                    } else if !matches!(*slice.index_set, Ty::Hole | Ty::Int(_)) {
                        ty_stack.last_mut().unwrap().0 = 2;
                        ty_stack.push((usize::MAX, slice.index_set.clone()));
                        break;
                    }
                }
                Ty::Reference(r) => {
                    if state == 1 {
                        r.referenced_ty = last_ty;
                    } else if !matches!(*r.referenced_ty, Ty::Hole | Ty::Int(_)) {
                        ty_stack.last_mut().unwrap().0 = 1;
                        ty_stack.push((usize::MAX, r.referenced_ty.clone()));
                        break;
                    }
                }
                Ty::Union(union) => {
                    for (i, sub_ty) in union.tys.iter().enumerate() {
                        if !matches!(**sub_ty, Ty::Hole | Ty::Int(_)) {
                            ty_stack.last_mut().unwrap().0 = i;
                            ty_stack.push((usize::MAX, sub_ty.clone()));
                        }
                    }
                }
                Ty::LateBound(_) => todo!(),
            };
            (_, last_ty) = ty_stack.pop().unwrap();
        }

        last_ty
    }
}

impl Scope {
    pub fn get_local(&self, name: Symbol) -> Option<&VarDef> {
        self.locals
            .iter()
            .rev()
            .find(|(l_name, _)| *l_name == name)
            .map(|(_, var)| var)
    }

    pub fn get_local_mut(&mut self, name: Symbol) -> Option<&mut VarDef> {
        self.locals
            .iter_mut()
            .rev()
            .find(|(l_name, _)| *l_name == name)
            .map(|(_, var)| var)
    }

    pub fn get_ty(&self, name: Symbol) -> Option<&TyDef> {
        self.tys.iter().rev().find(|ty| ty.name == name)
    }

    pub fn get_ty_mut(&mut self, name: Symbol) -> Option<&mut TyDef> {
        self.tys.iter_mut().rev().find(|ty| ty.name == name)
    }
}

/// Trait implemented on AST nodes to perform type synthesis, within the context of a program.
/// Type synthesis is the process of determining the smallest possible type that encapsulates all possible values of an expression.
/// For example:
///     synthesize_ty(`1 + 1`) -> `{2}`
///     synthesize_ty(`let x: Uint32; x + 1`) -> `Uint32`
#[allow(
    dead_code,
    reason = "Not sure if this will be exported yet, its more of a prototype"
)]
trait SynthesizeTy<L> {
    fn synthesize_ty(self, ctx: &LangCtx<L>) -> Rc<Ty<Symbol>>;
}

/// Trait implemented on AST nodes that don't need any outer context to perform type synthesis.
#[allow(
    dead_code,
    reason = "Not sure if this will be exported yet, its more of a prototype"
)]
trait SynthesizeTyPure {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>>;
}

impl<T: SynthesizeTyPure, L> SynthesizeTy<L> for T {
    fn synthesize_ty(self, _: &LangCtx<L>) -> Rc<Ty<Symbol>> {
        self.synthesize_ty_pure()
    }
}

impl SynthesizeTy<Span> for BoxAstNode {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let AstNode { data, span } = self.into_inner();
        match data {
            AstNodeData::LiteralInteger(n) => n.synthesize_ty(ctx),
            AstNodeData::LiteralString(n) => AstNode::new_narrow(span, &n).synthesize_ty(ctx),
            AstNodeData::LiteralChar(n) => AstNode::new_narrow(span, &n).synthesize_ty(ctx),
            AstNodeData::ExprInfix(n) => {
                // this is a really, really bad place to clone
                // if test performance is ever an issue LOOK HERE!
                AstNode::new_narrow(span, &n.map(|c| c.synthesize_ty(ctx))).synthesize_ty(ctx)
            }
            AstNodeData::LiteralStruct(n) => AstNode::new_narrow(span, n).synthesize_ty(ctx),
            AstNodeData::LiteralArray(n) => {
                AstNode::new_narrow(span, n.map(|c| c.synthesize_ty(ctx))).synthesize_ty(ctx)
            }
            AstNodeData::ExprLet(n) => {
                AstNode::new_narrow(span, n.map(|c| c.synthesize_ty(ctx))).synthesize_ty(ctx)
            }
            // AstNodeData::Block(n) => n.synthesize_ty(ctx),
            t => todo!("ty not implemented for test checker: {:?}", t),
        }
    }
}

impl SynthesizeTyPure for LiteralInteger {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        Rc::new(Ty::Int(TyInt::single(self.value)))
    }
}

impl SynthesizeTyPure for AstNode<&LiteralString> {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        let bytes = self.data.value.as_bytes();
        let values = IntegerSet::new_from_individual_generic(bytes);
        let element_ty = Rc::new(Ty::Int(TyInt {
            values,
            storage: StorageClass::unsigned(8),
        }));

        Rc::new(Ty::Array(TyArray {
            length: self.data.value.len(),
            element_ty,
        }))
    }
}

impl SynthesizeTyPure for &AstNode<&LiteralChar> {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        let c = self.data.value;
        let values = IntegerSet::new_from_individual(&[c as i128]);
        Rc::new(Ty::Int(TyInt {
            values,
            storage: StorageClass::unsigned(32),
        }))
    }
}

impl SynthesizeTyPure for AstNode<LiteralArray<Rc<Ty<Symbol>>>> {
    fn synthesize_ty_pure(self) -> Rc<Ty<Symbol>> {
        let union = TyUnion {
            tys: SmallVec::from(self.data.values.as_ref()),
        };

        Rc::new(Ty::Array(TyArray {
            length: self.data.values.len(),
            element_ty: Rc::new(Ty::Union(union)),
        }))
    }
}

impl SynthesizeTy<Span> for AstNode<LiteralStruct<BoxAstNode>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let ty = types::TyStruct {
            fields: self
                .data
                .members
                .into_iter()
                .map(|child| {
                    if let AstNodeData::LiteralStructMember(m) = child.into_inner().data {
                        (m.field, m.value.synthesize_ty(ctx))
                    } else {
                        panic!("child was not a struct member, this should be unreachable!")
                    }
                })
                .map(|(field, value_ty)| types::StructField {
                    name: ctx.symbols.intern(field.as_str()),
                    ty: value_ty.clone(),
                })
                .collect(),
        };

        Rc::new(Ty::Struct(ty))
    }
}

impl SynthesizeTy<Span> for AstNode<&ExprInfix<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        match self.data.op {
            InfixOp::Add => match self.data.lhs.arith_add(&self.data.rhs) {
                Ok(v) => Rc::new(v),
                Err(e) => {
                    ctx.error(CompilationError {
                        location: self.span,
                        kind: e.into(),
                    });
                    Rc::new(Ty::Hole)
                }
            },
            _ => todo!(),
        }
    }
}

impl SynthesizeTy<Span> for AstNode<ExprLet<Rc<Ty<Symbol>>>> {
    fn synthesize_ty(self, ctx: &LangCtx<Span>) -> Rc<Ty<Symbol>> {
        let var_symbol = ctx.symbols.intern(self.data.name.as_str());
        let var_ty = self.data.ty;
        let var_value_ty = self.data.value;
        ctx.var_def(
            ctx.root_scope_id,
            var_symbol,
            VarDef {
                assumed_ty: var_ty,
                last_assignment: var_value_ty.clone(),
            },
        );

        var_value_ty
    }
}

#[cfg(test)]
mod test {
    use crate::typetree::SynthesizeTy;

    use super::LangCtx;
    use howlite_syntax::{
        ast::{
            BoxAstNode, LiteralArray, LiteralChar, LiteralInteger, LiteralString, LiteralStruct,
            LiteralStructMember,
        },
        Span,
    };
    use howlite_typecheck::types::StorageClass;
    use preseli::IntegerSet;
    use prop::{sample::SizeRange, string::StringParam};
    use proptest::prelude::*;
    use smol_str::ToSmolStr;
    use sunstone::ops::{SetOpIncludeExclude, SetOpIncludes};

    /// Any literal that cannot contain arbirary data
    fn any_atomic_literal() -> impl Strategy<Value = BoxAstNode> {
        prop_oneof![
            any::<LiteralChar>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)),
            any::<LiteralString>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)),
            any::<LiteralInteger>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v))
        ]
    }

    fn literal_struct_member<K, V>(k: K, v: V) -> impl Strategy<Value = BoxAstNode>
    where
        K: Strategy<Value = String>,
        V: Strategy<Value = BoxAstNode>,
    {
        (k, v).prop_map(|(field, value)| {
            BoxAstNode::new(
                Span::new(0, 0),
                LiteralStructMember {
                    field: field.into(),
                    value,
                },
            )
        })
    }

    fn literal_struct<K, V, S>(k: K, v: V, length: S) -> impl Strategy<Value = BoxAstNode>
    where
        K: Strategy<Value = String>,
        V: Strategy<Value = BoxAstNode>,
        S: Into<SizeRange>,
    {
        proptest::collection::vec(literal_struct_member(k, v), length).prop_map(|members| {
            BoxAstNode::new(
                Span::new(0, 0),
                LiteralStruct {
                    members: members.into_iter().collect(),
                },
            )
        })
    }

    fn literal_array<V, S>(v: V, length: S) -> impl Strategy<Value = BoxAstNode>
    where
        V: Strategy<Value = BoxAstNode>,
        S: Into<SizeRange>,
    {
        proptest::collection::vec(v, length).prop_map(|members| {
            BoxAstNode::new(
                Span::new(0, 0),
                LiteralArray {
                    values: members.into_iter().collect(),
                },
            )
        })
    }

    fn any_literal() -> impl Strategy<Value = BoxAstNode> {
        any_atomic_literal().prop_recursive(4, 32, 12, |inner| {
            prop_oneof![
                literal_struct(any_ident(), inner.clone(), 0..12),
                literal_array(inner, 0..12),
            ]
        })
    }

    fn any_ident() -> impl Strategy<Value = String> {
        any_with::<String>(StringParam::from("[_a-zA-Z][_a-zA-Z0-9]*"))
    }

    proptest! {


        #[test]
        fn synthesize_literal_string(s in any::<String>()) {
            let lang = LangCtx::<Span>::new();
            let program = BoxAstNode::new(Span::new(0,0), LiteralString { value: s.to_smolstr() });
            let ty = program.synthesize_ty(&lang);
            let arr = ty.as_array().expect("string didn't synthesize to array");
            assert_eq!(arr.length, s.len());
            let elem_ty = arr.element_ty.as_int().expect("element ty was not an int");
            assert_eq!(elem_ty.storage, StorageClass::unsigned(8));
            for byte in s.bytes() {
                assert!(elem_ty.values.includes(byte as i128), "string char type not include {:#02x}", byte);
            }
        }

        #[test]
        fn synthesize_literal_char(c in any::<char>()) {
            let lang = LangCtx::<Span>::new();
            let program = BoxAstNode::new(Span::new(0,0), LiteralChar { value: c });
            let ty = program.synthesize_ty(&lang);
            let int = ty.as_int().expect("char didn't synthesize to int");
            assert!(int.values.includes(c as i128));
            assert_eq!(int.storage, StorageClass::unsigned(32));
            assert_eq!({let mut empty = int.values.clone(); empty.exclude_mut(&(c as i128)) ; empty}, IntegerSet::empty());
        }


        #[test]
        fn synthesize_literal_struct(program in literal_struct(any_ident(), any_literal(), 0..24)) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert!(ty.as_struct().is_some(), "expected struct type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_literal_array(program in literal_array(any_literal(), 0..10)) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert!(ty.as_array().is_some(), "expected array type, got: {:?}", ty);
        }

        #[test]
        fn synthesize_literal_array_large_int_table(program in literal_array(any::<LiteralInteger>().prop_map(|v| BoxAstNode::new(Span::new(0, 0), v)), 512..1024)) {
            let lang = LangCtx::<Span>::new();
            let ty = program.synthesize_ty(&lang);
            assert!(ty.as_array().is_some(), "expected array type, got: {:?}", ty);
        }
    }
}
