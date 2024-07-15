use std::collections::BTreeSet;

use super::types::{ArrayType, RecordCell, RecordType, TyRef};
use super::typetree::{GetTyData, GetTyDataPure};
use super::typetree::{TyContext, TypeTreeXData};
use super::{TypeError, TypeInfo};
use crate::parser::ast::{self, SyntaxData, SyntaxTree};

// Types
impl GetTyDataPure for ast::TyUnit {
    fn get_ty_data_pure(&self) -> TypeTreeXData {
        TypeTreeXData::new(TypeInfo::Unit)
    }
}

impl GetTyDataPure for ast::TyBool {
    fn get_ty_data_pure(&self) -> TypeTreeXData {
        TypeTreeXData::new(TypeInfo::bool())
    }
}

impl GetTyData for ast::TyParam {
    fn get_ty_data(
        &self,
        tree: &SyntaxTree,

        ty_database: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        ty_database.get(self.super_ty).clone()
    }
}

impl GetTyData for ast::TyRef {
    fn get_ty_data(
        &self,
        tree: &SyntaxTree,

        ty_database: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        TypeTreeXData::new(TypeInfo::TyRef(TyRef {
            parameters: self
                .parameters
                .iter()
                .map(|&x| ty_database.get(x).current_type())
                .cloned()
                .collect(),
            name: self.name.clone(),
        }))
    }
}

impl GetTyData for ast::StructMember {
    fn get_ty_data(
        &self,
        _tree: &SyntaxTree,
        db: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        // struct member's are assigned a type by the child node, we just inherit that constraint here for convienence.
        db.get(self.ty).clone()
    }
}

impl GetTyData for ast::TyArray {
    fn get_ty_data(
        &self,
        tree: &SyntaxTree,
        db: &SyntaxData<TypeTreeXData>,
        ctx: &mut TyContext,
    ) -> TypeTreeXData {
        let element_ty_data = db.get(self.element_ty);
        let ty = TypeInfo::Array(ArrayType {
            length: self.length,
            element_ty: Box::new(element_ty_data.current_type().clone()),
        });
        TypeTreeXData::new(ty)
    }
}

impl GetTyData for ast::TyStruct {
    fn get_ty_data(
        &self,
        tree: &SyntaxTree,
        tydb: &SyntaxData<TypeTreeXData>,
        ctx: &mut TyContext,
    ) -> TypeTreeXData {
        let mut fields = BTreeSet::new();
        let mut offset = 0;
        for &member in &self.members {
            // use xdata member here since this isn't an ADT ast type
            let type_info = tydb.get(member).current_type();
            let size = type_info.get_size();
            fields.insert(RecordCell {
                name: tree
                    .get(member)
                    .as_struct_member()
                    .expect("expected StructMember as child node to all structs")
                    .name
                    .clone(),
                offset,
                length: size,
                type_info: type_info.clone(),
            });
            offset += size
        }
        TypeTreeXData::new(TypeInfo::record(fields))
    }
}

// literals

impl GetTyData for ast::StructLiteral {
    fn get_ty_data(
        &self,
        tree: &SyntaxTree,
        tydb: &SyntaxData<TypeTreeXData>,
        ctx: &mut TyContext,
    ) -> TypeTreeXData {
        let fields = self.members.iter().map(|&member_ref| RecordCell {
            name: tree
                .get(member_ref)
                .as_struct_literal_member()
                .expect("all children of records must be 'StructMember'")
                .field
                .symbol
                .clone(),
            offset: 0, // TODO calc offset and length
            length: 0,
            type_info: tydb.get(member_ref).current_type().clone(),
        });

        TypeTreeXData::new(TypeInfo::record(fields.collect::<BTreeSet<_>>()))
    }
}

impl GetTyData for ast::StructLiteralMember {
    fn get_ty_data(
        &self,
        _tree: &SyntaxTree,
        tydb: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        tydb.get(self.value).clone()
    }
}

impl GetTyDataPure for ast::LiteralInteger {
    fn get_ty_data_pure(&self) -> TypeTreeXData {
        TypeTreeXData::new_value(TypeInfo::integer(self.value, self.value))
    }
}

impl GetTyDataPure for ast::LiteralBool {
    fn get_ty_data_pure(&self) -> TypeTreeXData {
        TypeTreeXData::new_value(TypeInfo::bool_valued(self.value))
    }
}

// expressions

impl GetTyData for ast::FieldAccess {
    fn get_ty_data(
        &self,
        _tree: &SyntaxTree,
        tydb: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        let object_ty = tydb.get(self.object);
        let field_name = self.field.symbol.clone();
        let field = object_ty.current_type().access(&field_name);
        match field {
            Ok(field) => TypeTreeXData::new(field),
            Err(e) => TypeTreeXData::new_error(TypeInfo::Unit, e),
        }
    }
}

// definitions

impl GetTyData for ast::DefExtern {
    fn get_ty_data(
        &self,
        _tree: &SyntaxTree,
        tydb: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        let params: Vec<_> = self
            .params
            .iter()
            .map(|&x| tydb.get(x).current_type())
            .cloned()
            .collect();
        let ret = tydb.get(self.return_ty);
        TypeTreeXData::new(TypeInfo::func(params, ret.current_type().clone()))
    }
}

impl GetTyData for ast::DefFunction {
    fn get_ty_data(
        &self,
        _tree: &SyntaxTree,
        tydb: &SyntaxData<TypeTreeXData>,
        ctx: &mut TyContext,
    ) -> TypeTreeXData {
        let param_tys: Vec<TypeInfo> = Vec::with_capacity(self.params.len());
        let return_ty = tydb.get(self.return_ty).current_type();
        ctx.with_scope(|| {
            let body_ty = tydb.get(self.body).current_type();
            TypeTreeXData::new(TypeInfo::func(params, return_ty.current_type().clone()))
        })
    }
}

impl GetTyData for ast::Param {
    fn get_ty_data(
        &self,
        _tree: &SyntaxTree,
        tydb: &SyntaxData<TypeTreeXData>,
        _ctx: &mut TyContext,
    ) -> TypeTreeXData {
        tydb.get(self.typ).clone()
    }
}

// top-level
impl GetTyDataPure for ast::Program {
    fn get_ty_data_pure(&self) -> TypeTreeXData {
        Default::default()
    }
}
