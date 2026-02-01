use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use adam_ir::ir::IrType;

use crate::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    /// Allocate space on the stack for a local variable.
    pub fn stack_alloc(&self, ty: &IrType, name: &str) -> PointerValue<'ctx> {
        let llvm_ty = self.llvm_type(ty);
        self.builder.build_alloca(llvm_ty, name).expect("failed to build alloca")
    }

    /// Generate a heap allocation: calls `__adam_alloc(size, align)` → ptr.
    pub fn heap_alloc(&self, ty: &IrType) -> PointerValue<'ctx> {
        let size = self.size_of_type(ty);
        let align = self.align_of_type(ty);

        let alloc_fn = self.get_or_declare_alloc();
        let size_val = self.context.i64_type().const_int(size, false);
        let align_val = self.context.i64_type().const_int(align, false);

        let result = self.builder
            .build_call(alloc_fn, &[size_val.into(), align_val.into()], "heap_ptr")
            .expect("failed to build alloc call");

        match result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => val.into_pointer_value(),
            inkwell::values::ValueKind::Instruction(_) => {
                self.context.ptr_type(AddressSpace::default()).const_null()
            }
        }
    }

    /// Generate a heap deallocation: calls `__adam_dealloc(ptr, size, align)`.
    pub fn heap_dealloc(&self, ptr: PointerValue<'ctx>, ty: &IrType) {
        let size = self.size_of_type(ty);
        let align = self.align_of_type(ty);

        let dealloc_fn = self.get_or_declare_dealloc();
        let size_val = self.context.i64_type().const_int(size, false);
        let align_val = self.context.i64_type().const_int(align, false);

        self.builder
            .build_call(dealloc_fn, &[ptr.into(), size_val.into(), align_val.into()], "")
            .expect("failed to build dealloc call");
    }

    /// Generate a drop call for a variable — dispatches based on type.
    pub fn codegen_drop(&self, ptr: PointerValue<'ctx>, ty: &IrType) {
        match ty {
            // Primitives: no drop needed.
            IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64
            | IrType::U8 | IrType::U16 | IrType::U32 | IrType::U64
            | IrType::F32 | IrType::F64
            | IrType::Bool | IrType::Char | IrType::Unit | IrType::Void => {}

            // String: call __adam_string_drop(ptr)
            IrType::String => {
                let drop_fn = self.get_or_declare_void_fn(
                    "__adam_string_drop",
                    &[self.context.ptr_type(AddressSpace::default()).into()],
                );
                self.builder
                    .build_call(drop_fn, &[ptr.into()], "")
                    .expect("failed to build string drop");
            }

            // Dynamic array: call __adam_vec_drop(ptr)
            IrType::Array(_, None) => {
                let drop_fn = self.get_or_declare_void_fn(
                    "__adam_vec_drop",
                    &[self.context.ptr_type(AddressSpace::default()).into()],
                );
                self.builder
                    .build_call(drop_fn, &[ptr.into()], "")
                    .expect("failed to build vec drop");
            }

            // Heap pointer: dealloc the pointee
            IrType::Ptr(inner) => {
                let pointee = self.builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        ptr,
                        "load_ptr",
                    )
                    .expect("load ptr");
                self.heap_dealloc(pointee.into_pointer_value(), inner);
            }

            // Channel: call __adam_chan_drop(ptr)
            IrType::Channel(_) => {
                let drop_fn = self.get_or_declare_void_fn(
                    "__adam_chan_drop",
                    &[self.context.ptr_type(AddressSpace::default()).into()],
                );
                self.builder
                    .build_call(drop_fn, &[ptr.into()], "")
                    .expect("failed to build chan drop");
            }

            // Fixed array, tuple, struct, enum: drop each field recursively.
            // This will be elaborated when struct layout info is available.
            _ => {}
        }
    }

    /// Build a `memcpy(dst, src, size)` call.
    pub fn build_memcpy(
        &self,
        dst: PointerValue<'ctx>,
        src: PointerValue<'ctx>,
        size: u64,
    ) {
        let memcpy_fn = self.get_or_declare_void_fn(
            "llvm.memcpy.p0.p0.i64",
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
                self.context.bool_type().into(),
            ],
        );

        let size_val = self.context.i64_type().const_int(size, false);
        let is_volatile = self.context.bool_type().const_zero();

        self.builder
            .build_call(memcpy_fn, &[dst.into(), src.into(), size_val.into(), is_volatile.into()], "")
            .expect("failed to build memcpy");
    }

    // ================================================================
    // Internal helpers — get or declare runtime functions
    // ================================================================

    fn get_or_declare_alloc(&self) -> inkwell::values::FunctionValue<'ctx> {
        let name = "__adam_alloc";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.ptr_type(AddressSpace::default()).fn_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }

    fn get_or_declare_dealloc(&self) -> inkwell::values::FunctionValue<'ctx> {
        let name = "__adam_dealloc";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.void_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }

    /// Get or declare a void-returning function with the given name and param types.
    pub(crate) fn get_or_declare_void_fn(
        &self,
        name: &str,
        param_types: &[inkwell::types::BasicMetadataTypeEnum<'ctx>],
    ) -> inkwell::values::FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.void_type().fn_type(param_types, false);
        self.module.add_function(name, fn_type, Some(inkwell::module::Linkage::External))
    }
}
