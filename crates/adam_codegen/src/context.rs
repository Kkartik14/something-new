use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::basic_block::BasicBlock;
use inkwell::OptimizationLevel;

use adam_ir::ir::{BlockId, FnId, VarId};

pub struct CodeGen<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) target_machine: TargetMachine,

    // Mappings from Adam IR to LLVM values
    pub(crate) functions: HashMap<FnId, FunctionValue<'ctx>>,
    pub(crate) variables: HashMap<VarId, PointerValue<'ctx>>,
    pub(crate) blocks: HashMap<BlockId, BasicBlock<'ctx>>,
    pub(crate) strings: HashMap<u32, PointerValue<'ctx>>,

    // Track the actual LLVM type stored in each variable (may differ from declared IR type).
    pub(crate) var_value_types: HashMap<VarId, BasicTypeEnum<'ctx>>,

    // Named struct types (populated during codegen_module from struct_defs).
    pub(crate) struct_types: HashMap<String, StructType<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        // Initialize native target
        Target::initialize_native(&InitializationConfig::default())
            .expect("failed to initialize native target");

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).expect("failed to get target from triple");
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();

        let target_machine = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("failed to create target machine");

        let data_layout = target_machine.get_target_data().get_data_layout();
        module.set_data_layout(&data_layout);
        module.set_triple(&triple);

        CodeGen {
            context,
            module,
            builder,
            target_machine,
            functions: HashMap::new(),
            variables: HashMap::new(),
            blocks: HashMap::new(),
            strings: HashMap::new(),
            var_value_types: HashMap::new(),
            struct_types: HashMap::new(),
        }
    }

    /// Create a new CodeGen targeting a specific triple (for cross-compilation).
    pub fn with_triple(context: &'ctx Context, module_name: &str, triple: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Target::initialize_all(&InitializationConfig::default());

        let triple = TargetTriple::create(triple);
        let target = Target::from_triple(&triple).expect("failed to get target from triple");

        let target_machine = target
            .create_target_machine(
                &triple,
                "",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("failed to create target machine");

        let data_layout = target_machine.get_target_data().get_data_layout();
        module.set_data_layout(&data_layout);
        module.set_triple(&triple);

        CodeGen {
            context,
            module,
            builder,
            target_machine,
            functions: HashMap::new(),
            variables: HashMap::new(),
            blocks: HashMap::new(),
            strings: HashMap::new(),
            var_value_types: HashMap::new(),
            struct_types: HashMap::new(),
        }
    }

    /// Write the LLVM IR to a string (for debugging/testing).
    pub fn print_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Write the LLVM bitcode to a file.
    pub fn write_bitcode_to_file(&self, path: &std::path::Path) -> bool {
        self.module.write_bitcode_to_path(path)
    }

    /// Emit an object file (.o) for the current module.
    pub fn emit_object_file(&self, path: &std::path::Path) -> Result<(), String> {
        self.target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| e.to_string())
    }

    /// Verify the LLVM module for correctness.
    pub fn verify(&self) -> Result<(), String> {
        self.module.verify().map_err(|e| e.to_string())
    }
}
