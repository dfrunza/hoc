#pragma once

typedef enum
{
  Opcode__Null,
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
  Opcode_INCR,
  Opcode_DECR,
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
  Opcode_ENTER,
  Opcode_LEAVE,
  Opcode_ALLOC,
}
Opcode;

typedef struct
{
  int sourceLineNr;
  char* string;
}
InstructionLine;

typedef enum
{
  ParamType__Null,
  ParamType_Int32,
  ParamType_String,
  ParamType_Reg,
}
ParamType;

typedef enum
{
  RegName__Null,
  RegName_IP,
  RegName_SP,
  RegName_FP
}
RegName;

typedef struct
{
  Opcode    opcode;
  ParamType paramType;

  union {
    int32   intNum;
    RegName reg;
    char*   str;
  } param;

  int sourceLineNr;
}
Instruction;

typedef struct
{
  char groove[4];
  int instrCount;
  uint8* codeStart;
  int codeSize;
  Instruction* instrArray;
}
IrCode;
