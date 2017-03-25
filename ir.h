#pragma once

enum struct Opcode
{
  _Null,
  PUSH,
  POP,
  LOAD,
  LOAD8,
  STORE,
  STORE8,
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  INCR,
  DECR,
  JUMPNZ,
  JUMPZ,
  GOTO,
  HALT,
  PRINT,
  DUP,
  LABEL,
  NOOP,
  CALL,
  RETURN,
  ENTER,
  LEAVE,
  ALLOC,
};

struct InstructionLine
{
  int sourceLineNr;
  char* string;
};

enum struct ParamType
{
  _Null,
  Int32,
  String,
  Reg,
};

enum struct RegName
{
  _Null,
  IP,
  SP,
  FP
};

struct Instruction
{
  Opcode    opcode;
  ParamType paramType;

  union {
    int32   intNum;
    RegName reg;
    char*   str;
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
