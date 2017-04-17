#pragma once

typedef enum
{
  Opcode__Null,
  Opcode_PUSH,
  Opcode_POP,
  Opcode_DUP,
  Opcode_LOAD,
  Opcode_LOAD8,
  Opcode_STORE,
  Opcode_STORE8,
  Opcode_ALLOC,

  Opcode_ADD,
  Opcode_SUB,
  Opcode_MUL,
  Opcode_DIV,
  Opcode_MOD,
  Opcode_NEG,
  Opcode_INCR,
  Opcode_DECR,

  Opcode_CMPEQ,
  Opcode_CMPNEQ,
  Opcode_CMPLSS,
  Opcode_CMPGRT,
  Opcode_AND,
  Opcode_OR,
  Opcode_NOT,

  Opcode_LABEL,
  Opcode_JUMPZ,
  Opcode_JUMPNZ,
  Opcode_GOTO,
  Opcode_CALL,
  Opcode_RETURN,
  Opcode_ENTER,
  Opcode_LEAVE,
  Opcode_HALT,

  Opcode_NOOP,
  Opcode_PRINT,
  Opcode_PRINTNL,
}
Opcode;

typedef struct
{
  int   source_line_nr;
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
  ParamType param_type;

  union {
    int32   int_num;
    RegName reg;
    char*   str;
  } param;

  int source_line_nr;
}
Instruction;

typedef struct
{
  char         groove[4];
  int          instr_count;
  uint8*       code_start;
  uint         code_size;
  Instruction* instr_array;
}
HasmCode;

