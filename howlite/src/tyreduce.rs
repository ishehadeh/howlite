use howlite_syntax::{ast::*, tree::NodeId};

pub struct TyContext;

pub struct TyTreeReducer;

impl FoldTree for TyTreeReducer {
    type Value = TyContext;

    fn literal_integer(&self, _id: NodeId<AstNode>, _node: &LiteralInteger) -> Self::Value {
        todo!()
    }

    fn literal_char(&self, _id: NodeId<AstNode>, _node: &LiteralChar) -> Self::Value {
        todo!()
    }

    fn literal_string(&self, _id: NodeId<AstNode>, _node: &LiteralString) -> Self::Value {
        todo!()
    }

    fn literal_array<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &LiteralArray<A>,
        _children: LiteralArrayChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn literal_struct<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &LiteralStruct<A>,
        _children: LiteralStructChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn literal_struct_member(
        &self,
        _id: NodeId<AstNode>,
        _node: &LiteralStructMember,
        _children: LiteralStructMemberChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ident(&self, _id: NodeId<AstNode>, _node: &Ident) -> Self::Value {
        todo!()
    }

    fn field_access(
        &self,
        _id: NodeId<AstNode>,
        _node: &FieldAccess,
        _children: FieldAccessChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn array_access(
        &self,
        _id: NodeId<AstNode>,
        _node: &ArrayAccess,
        _children: ArrayAccessChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn repaired(
        &self,
        _id: NodeId<AstNode>,
        _node: &Repaired,
        _children: RepairedChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn def_func<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &DefFunc<A>,
        _children: DefFuncChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn def_param(
        &self,
        _id: NodeId<AstNode>,
        _node: &DefParam,
        _children: DefParamChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn def_import(&self, _id: NodeId<AstNode>, _node: &DefImport) -> Self::Value {
        todo!()
    }

    fn block<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &Block<A>,
        _children: BlockChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn expr_if(
        &self,
        _id: NodeId<AstNode>,
        _node: &ExprIf,
        _children: ExprIfChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn expr_call<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &ExprCall<A>,
        _children: ExprCallChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn expr_infix(
        &self,
        _id: NodeId<AstNode>,
        _node: &ExprInfix,
        _children: ExprInfixChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn expr_prefix(
        &self,
        _id: NodeId<AstNode>,
        _node: &ExprPrefix,
        _children: ExprPrefixChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn expr_type_construction(
        &self,
        _id: NodeId<AstNode>,
        _node: &ExprTypeConstruction,
        _children: ExprTypeConstructionChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn expr_let(
        &self,
        _id: NodeId<AstNode>,
        _node: &ExprLet,
        _children: ExprLetChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn expr_while(
        &self,
        _id: NodeId<AstNode>,
        _node: &ExprWhile,
        _children: ExprWhileChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn def_type<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &DefType<A>,
        _children: DefTypeChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn def_extern_func<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &DefExternFunc<A>,
        _children: DefExternFuncChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn def_extern_var(
        &self,
        _id: NodeId<AstNode>,
        _node: &DefExternVar,
        _children: DefExternVarChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn program<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &Program<A>,
        _children: ProgramChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_ref(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyRef,
        _children: TyRefChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_expr_union(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyExprUnion,
        _children: TyExprUnionChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_struct<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyStruct<A>,
        _children: TyStructChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_struct_member(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyStructMember,
        _children: TyStructMemberChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_number_range(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyNumberRange,
        _children: TyNumberRangeChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_array(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyArray,
        _children: TyArrayChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_unit(&self, _id: NodeId<AstNode>, _node: &TyUnit) -> Self::Value {
        todo!()
    }

    fn ty_param(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyParam,
        _children: TyParamChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_slice(
        &self,
        _id: NodeId<AstNode>,
        _node: &TySlice,
        _children: TySliceChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }

    fn ty_named<A: allocator_api2::alloc::Allocator>(
        &self,
        _id: NodeId<AstNode>,
        _node: &TyNamed<A>,
        _children: TyNamedChildren<Self::Value>,
    ) -> Self::Value {
        todo!()
    }
}
