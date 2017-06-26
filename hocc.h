#pragma once
#include "lib.h"
#include "hasm.c"
#include "semantic.h"

// User-defined PE resource:
//   nameId  typeId  fileName
#define OUT_RC "CODE  VM  \"%s\""

typedef struct
{
  char* name;
  int len;
}
FileName;

typedef struct
{
  char strings[4*80 + 4*10];
  FileName ir;
  FileName irc;
  FileName rc;
  FileName res;
}
OutFileNames;

typedef struct
{
  String text;
  int text_len;
  List instr_list;
  bool success;
}
VmProgram;

