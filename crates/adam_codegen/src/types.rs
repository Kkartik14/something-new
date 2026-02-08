use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::AddressSpace;

use adam_ir::ir::IrType;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Map an Adam IR type to an LLVM basic type.
    pub fn llvm_type(&self, ty: &IrType) -> BasicTypeEnum<'ctx> {
        match ty {
            IrType::I8 | IrType::U8 => self.context.i8_type().into(),
            IrType::I16 | IrType::U16 => self.context.i16_type().into(),
            IrType::I32 | IrType::U32 => self.context.i32_type().into(),
            IrType::I64 | IrType::U64 => self.context.i64_type().into(),
            IrType::F32 => self.context.f32_type().into(),
            IrType::F64 => self.context.f64_type().into(),
            IrType::Bool => self.context.bool_type().into(),
            IrType::Char => self.context.i32_type().into(), // Unicode scalar value
            IrType::Unit => self.unit_type().into(),

            // String: { i8*, i64, i64 } — ptr, len, capacity
            IrType::String => self.string_type().into(),

            // str (slice): { i8*, i64 } — ptr, len
            IrType::Str => self.str_slice_type().into(),

            // Pointer / reference
            IrType::Ptr(_) => self.context.ptr_type(AddressSpace::default()).into(),

            // Dynamic array: { T*, i64, i64 } — ptr, len, capacity
            IrType::Array(_, None) => self.string_type().into(), // same layout as Vec

            // Fixed-size array: [N x T]
            IrType::Array(elem_ty, Some(n)) => {
                let elem = self.llvm_type(elem_ty);
                elem.array_type(*n as u32).into()
            }

            // Tuple: anonymous struct { T0, T1, ... }
            IrType::Tuple(fields) => {
                let field_types: Vec<BasicTypeEnum<'ctx>> =
                    fields.iter().map(|f| self.llvm_type(f)).collect();
                self.context.struct_type(&field_types, false).into()
            }

            // Named struct — look up from the struct_types cache populated during codegen_module.
            IrType::Struct(name) => {
                if let Some(st) = self.struct_types.get(name) {
                    (*st).into()
                } else {
                    // Fallback: create opaque struct (will be filled in later)
                    let st = self.context.opaque_struct_type(name);
                    st.into()
                }
            }

            // Enum: tagged union { i8, union { variant_0, variant_1, ... } }
            // For now, represent as { i8, [max_variant_size x i8] }
            IrType::Enum(name) => {
                if let Some(st) = self.struct_types.get(name) {
                    (*st).into()
                } else {
                    let st = self.context.opaque_struct_type(name);
                    st.into()
                }
            }

            // Function pointer
            IrType::Function(_, _) => self.context.ptr_type(AddressSpace::default()).into(),

            // Channel: opaque pointer to runtime channel struct
            IrType::Channel(_) => self.context.ptr_type(AddressSpace::default()).into(),

            // Void should not appear as a basic type, but handle gracefully
            IrType::Void => self.unit_type().into(),
        }
    }

    /// Build an LLVM function type from Adam IR param/return types.
    pub fn llvm_fn_type(&self, params: &[IrType], ret: &IrType) -> FunctionType<'ctx> {
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            params.iter().map(|p| self.llvm_type(p).into()).collect();

        match ret {
            IrType::Void | IrType::Unit => self.context.void_type().fn_type(&param_types, false),
            _ => {
                let ret_type = self.llvm_type(ret);
                ret_type.fn_type(&param_types, false)
            }
        }
    }

    /// The Adam `Unit` type — represented as an empty struct `{}`.
    fn unit_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(&[], false)
    }

    /// The Adam `String` type: `{ i8*, i64, i64 }` (ptr, len, capacity).
    pub fn string_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        )
    }

    /// The Adam `str` slice type: `{ i8*, i64 }` (ptr, len).
    pub fn str_slice_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
            ],
            false,
        )
    }

    /// Define a named struct type with the given field types.
    pub fn define_struct_type(
        &self,
        name: &str,
        field_types: &[IrType],
    ) -> inkwell::types::StructType<'ctx> {
        let st = self.context.opaque_struct_type(name);
        let fields: Vec<BasicTypeEnum<'ctx>> =
            field_types.iter().map(|f| self.llvm_type(f)).collect();
        st.set_body(&fields, false);
        st
    }

    /// Define an enum type as a tagged union: `{ i8, [payload_size x i8] }`.
    pub fn define_enum_type(
        &self,
        name: &str,
        max_variant_size: u32,
    ) -> inkwell::types::StructType<'ctx> {
        let st = self.context.opaque_struct_type(name);
        let tag = self.context.i8_type();
        let payload = self.context.i8_type().array_type(max_variant_size);
        st.set_body(&[tag.into(), payload.into()], false);
        st
    }
}
