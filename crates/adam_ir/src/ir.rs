//! IR data structures — the mid-level intermediate representation.
//!
//! The Adam IR (AIR) is a control-flow graph (CFG) representation
//! organized into functions, basic blocks, instructions, and terminators.

pub type BlockId = u32;
pub type VarId = u32;
pub type FnId = u32;

#[derive(Debug, Clone)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
    pub globals: Vec<IrGlobal>,
    pub string_literals: Vec<String>,
    pub struct_defs: Vec<IrStructDef>,
}

#[derive(Debug, Clone)]
pub struct IrStructDef {
    pub name: String,
    pub fields: Vec<IrStructField>,
}

#[derive(Debug, Clone)]
pub struct IrStructField {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug, Clone)]
pub struct IrGlobal {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrType {
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Bool, Char, Unit,
    String, Str,
    Ptr(Box<IrType>),
    Array(Box<IrType>, Option<u64>),
    Tuple(Vec<IrType>),
    Struct(String),
    Enum(String),
    Function(Vec<IrType>, Box<IrType>),  // params, return
    Channel(Box<IrType>),
    Void,
}

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub id: FnId,
    pub name: String,
    pub params: Vec<IrParam>,
    pub return_type: IrType,
    pub blocks: Vec<BasicBlock>,
    pub entry: BlockId,
    pub locals: Vec<IrLocal>,
}

#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug, Clone)]
pub struct IrLocal {
    pub id: VarId,
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Assign(VarId, RValue),
    Drop(VarId),
    Nop,
}

#[derive(Debug, Clone)]
pub enum RValue {
    Use(Operand),
    BinaryOp(BinOp, Operand, Operand),
    UnaryOp(UnOp, Operand),
    Call(FnId, Vec<Operand>),
    CallNamed(String, Vec<Operand>),
    Aggregate(AggregateKind, Vec<Operand>),
    Field(Operand, u32),
    Index(Operand, Operand),
    Ref(VarId),
    MutRef(VarId),
    Deref(Operand),
    Cast(Operand, IrType),
    Constant(Constant),
    HeapAlloc(IrType),
    ChanCreate(IrType, Option<u64>),
    ChanSend(Operand, Operand),
    ChanRecv(Operand),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, NotEq, Lt, Gt, LtEq, GtEq,
    And, Or,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp { Neg, Not, Ref }

#[derive(Debug, Clone, PartialEq)]
pub enum AggregateKind {
    Array,
    Tuple,
    Struct(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Var(VarId),
    Constant(Constant),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(u32),  // index into string_literals
    Unit,
    Nil,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return(Option<Operand>),
    Goto(BlockId),
    Branch(Operand, BlockId, BlockId),
    Switch(Operand, Vec<(Constant, BlockId)>, BlockId),
    Spawn(BlockId, BlockId),  // (spawn_target, continuation)
    Select(Vec<SelectBranch>),
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct SelectBranch {
    pub kind: SelectBranchKind,
    pub target: BlockId,
}

#[derive(Debug, Clone)]
pub enum SelectBranchKind {
    Recv(VarId, Operand),
    Send(Operand, Operand),
    After(Operand),
}
