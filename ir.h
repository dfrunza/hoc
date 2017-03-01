enum Opcode
{
  Opcode__Nul,
  Opcode_PUSH,
  Opcode_POP,
  Opcode_LOAD,
  Opcode_LOAD8,
  Opcode_STORE,
  Opcode_STORE8,
  Opcode_ADD,
  Opcode_SUB,
  Opcode_MUL,
  Opcode_DIV,
  Opcode_MOD,
  Opcode_INC,
  Opcode_DEC,
  Opcode_JUMPNZ,
  Opcode_JUMPZ,
  Opcode_GOTO,
  Opcode_HALT,
  Opcode_PRINT,
  Opcode_DUP,
  Opcode_LABEL,
  Opcode_NOOP,
  Opcode_CALL,
  Opcode_RETURN,
  Opcode_ALLOC,
};

struct InstructionLine
{
  int sourceLineNr;
  char* string;
};

enum ParamType
{
  Param__Null,
  Param_Int32,
  Param_String,
  Param_Reg,
};

enum RegName
{
  Reg__Nul,
  Reg_IP,
  Reg_SP,
  Reg_FP
};

struct Instruction
{
  Opcode opcode;
  ParamType paramType;

  union {
    int32 intNum;
    RegName reg;
    char* str;
  } param;

  int sourceLineNr;
};

struct IrCode
{
  char groove[4];
  int instrCount;
  uint8* codeStart;
  int codeSize;
  Instruction* instrArray;
};
