#include <stdio.h>
#include "lib.cpp"

enum Opcode
{
  Opcode_NULL,
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

  Opcode__Count
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
};

struct Instruction
{
  Opcode opcode;
  ParamType paramType;

  union {
    int32 iParam;
    char* strParam;
  };
  InstructionLine* line;
};

struct Label
{
  char* string;
  int instrAddress;
};

struct SourceProgram
{
  char* text;

  int maxLines;
  int lineCount;
  InstructionLine lines[1024];

  int maxLabels;
  int labelCount;
  Label labels[512]; //FIXME: Unchecked bounds
};

#define VMWORD (sizeof(int32))

void Error(char* message, ...)
{
  va_list args;
  fprintf(stdout, "Error : ");

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

struct Machine
{
  SourceProgram* source;
  int32 memorySize;
  int32 sp;
  int32 fp;
  int32 ip;
  uint8 memory[2048];

  int32 instrArraySize;
  int32 instrCount;
  Instruction instrArray[1024];
};

enum ExecResult
{
  ExecResult_EndOfProgram = 0,
  ExecResult_OK,
  ExecResult_InvalidMemoryAccess,
  ExecResult_InvalidInstructionAddress,
  ExecResult_InvalidOperandSize,
  ExecResult_IllegalInstruction,
  ExecResult_DivByZero,

  ExecResult__Count
};

int BreakInstructionIntoComponents(char* str, char* components[], int maxComponentCount)
{/*>>>*/
  int componentCount = 0;

  char* component = str;
  char* charPtr = str;

  do
  {
    while(*charPtr != '\0' && *charPtr != ' ' && *charPtr != ';')
      charPtr++;

    if(componentCount < maxComponentCount)
      components[componentCount++] = component;
    else
      return maxComponentCount+1;

    if(*charPtr != '\0')
    {
      if(*charPtr == ';')
      {
        while(*charPtr != '\0')
          *charPtr++ = '\0';
      }
      else
      {
        while(*charPtr == ' ')
          *charPtr++ = '\0';
        component = charPtr;
      }
    }
  } while(*charPtr != '\0' && *charPtr != ';');

  return componentCount;
}/*<<<*/

void Error_InvalidInstruction()
{
  Error("Invalid instruction or instruction format");
}

int FindInstructionAddressAtLine(SourceProgram* source, int lineNr)
{
  int result = -1;
  InstructionLine* line = source->lines;
  for(int lineIndex = 0; lineIndex < source->lineCount; lineIndex++)
  {
    if(line->sourceLineNr == lineNr)
    {
      result = lineIndex;
      break;
    }
    line++;
  }
  return result;
}

Label* FindLabel(SourceProgram* source, char* labelString)
{
  Label* result = 0;
  for(int i = 0; i < source->labelCount; i++)
  {
    Label* label = &source->labels[i];
    if(StrEquals(label->string, labelString))
    {
      result = label;
      break;
    }
  }
  return result;
}

void ProcessSourceLines(SourceProgram* source)
{/*>>>*/
  source->lineCount = 0;
  char* charPtr = &source->text[0];
  if(*charPtr != '\0')
  {
    int sourceLineNr = 1;

    if(*charPtr != '\n' && *charPtr != ';')
    {
      InstructionLine instrLine = {};
      instrLine.sourceLineNr = sourceLineNr;
      instrLine.string = charPtr;
      source->lines[source->lineCount++] = instrLine;

      charPtr++;
      while(*charPtr != '\0')
      {
        if(*charPtr == '\n')
        {
          sourceLineNr++;

          char* nextCharPtr = charPtr+1;
          if(*nextCharPtr != '\0' && *nextCharPtr != '\n' && *nextCharPtr != ';')
          {
            instrLine = {};
            instrLine.sourceLineNr = sourceLineNr;
            instrLine.string = nextCharPtr;
            source->lines[source->lineCount++] = instrLine;
          }
          *charPtr = '\0';
        }
        charPtr++;
      }
    }
  }
}/*<<<*/

bool32 BuildInstructionArray(Machine* machine)
{/*>>>*/
  SourceProgram* source = machine->source;
  machine->instrCount = source->lineCount;

  for(int instrAddress = 0; instrAddress < machine->instrCount; instrAddress++)
  {
    Instruction instr = {};
    InstructionLine* instrLine = &source->lines[instrAddress];
    instr.line = instrLine;

    char* components[2] = {};
    int componentCount = BreakInstructionIntoComponents(instrLine->string,
                                                        components, SizeofArray(components));
    if(componentCount >= 1 && componentCount <= SizeofArray(components))
    {
      char* mnemonic = components[0];

      if(StrEquals(mnemonic, "pop"))
        instr.opcode = Opcode_POP;
      else if(StrEquals(mnemonic, "push"))
        instr.opcode = Opcode_PUSH;
      else if(StrEquals(mnemonic, "store"))
        instr.opcode = Opcode_STORE;
      else if(StrEquals(mnemonic, "store8"))
        instr.opcode = Opcode_STORE8;
      else if(StrEquals(mnemonic, "load"))
        instr.opcode = Opcode_LOAD;
      else if(StrEquals(mnemonic, "load8"))
        instr.opcode = Opcode_LOAD8;
      else if(StrEquals(mnemonic, "add"))
        instr.opcode = Opcode_ADD;
      else if(StrEquals(mnemonic, "sub"))
        instr.opcode = Opcode_SUB;
      else if(StrEquals(mnemonic, "mul"))
        instr.opcode = Opcode_MUL;
      else if(StrEquals(mnemonic, "div"))
        instr.opcode = Opcode_DIV;
      else if(StrEquals(mnemonic, "mod"))
        instr.opcode = Opcode_MOD;
      else if(StrEquals(mnemonic, "inc"))
        instr.opcode = Opcode_INC;
      else if(StrEquals(mnemonic, "dec"))
        instr.opcode = Opcode_DEC;
      else if(StrEquals(mnemonic, "halt"))
        instr.opcode = Opcode_HALT;
      else if(StrEquals(mnemonic, "print"))
        instr.opcode = Opcode_PRINT;
      else if(StrEquals(mnemonic, "dup"))
        instr.opcode = Opcode_DUP;
      else if(StrEquals(mnemonic, "goto"))
        instr.opcode = Opcode_GOTO;
      else if(StrEquals(mnemonic, "jumpnz"))
        instr.opcode = Opcode_JUMPNZ;
      else if(StrEquals(mnemonic, "jumpz"))
        instr.opcode = Opcode_JUMPZ;
      else if(StrEquals(mnemonic, "label"))
        instr.opcode = Opcode_LABEL;
      else if(StrEquals(mnemonic, "noop"))
        instr.opcode = Opcode_NOOP;
      else if(StrEquals(mnemonic, "call"))
        instr.opcode = Opcode_CALL;
      else if(StrEquals(mnemonic, "return"))
        instr.opcode = Opcode_RETURN;
      else if(StrEquals(mnemonic, "alloc"))
        instr.opcode = Opcode_ALLOC;
      else {
        Error_InvalidInstruction();
        return false;
      }

      if(componentCount == 2)
      {
        switch(instr.opcode)
        {
          case Opcode_GOTO:
          case Opcode_JUMPNZ:
          case Opcode_JUMPZ:
          case Opcode_CALL:
            {
              if(IsLetterChar(*components[1]))
              {
                instr.paramType = Param_String;
                instr.strParam = components[1];
              }
              else {
                Error("Label '%s' does not begin with a letter", components[1]);
                return false;
              }
            } break;

          case Opcode_POP:
          case Opcode_PUSH:
            {
              if(StrToInt(components[1], &instr.iParam))
                instr.paramType = Param_Int32;
              else {
                instr.paramType = Param_String;
                instr.strParam = components[1];

                if(!StrMatch(instr.strParam, "sp") &&
                   !StrMatch(instr.strParam, "fp") &&
                   !StrMatch(instr.strParam, "ip"))
                {
                  Error("Invalid register '%s'", components[1]);
                  return false;
                }
              }
            } break;

          case Opcode_ALLOC:
            {
              if(StrToInt(components[1], &instr.iParam))
                instr.paramType = Param_Int32;
              else {
                Error("Invalid parameter '%s'", components[1]);
                return false;
              }
            } break;

          case Opcode_LABEL:
            {
              if(IsLetterChar(*components[1]))
              {
                instr.paramType = Param_String;
                instr.strParam = components[1];

                Label label = {};
                label.string = components[1];
                label.instrAddress = instrAddress;

                if(!FindLabel(source, label.string))
                  source->labels[source->labelCount++] = label;
                else {
                  Error("Duplicate label declaration '%s'", label.string);
                  return false;
                }
              } else {
                Error("Label '%s' does not begin with a letter", components[1]);
                return false;
              }
            } break;

          default:
            Error("Incorrect number of parameters to instruction '%s'", mnemonic);
            return false;
        }
      }

      machine->instrArray[instrAddress] = instr;
    }
    else
    {
      Error("Illegal number of instruction components");
      return false;
    }
  }

  for(int labelIndex = 0; labelIndex < source->labelCount; labelIndex++)
  {
    Label* label = &source->labels[labelIndex];
    int targetInstrAddress = label->instrAddress+1;
    for(; targetInstrAddress < machine->instrCount; targetInstrAddress++)
    {
      Instruction* instr = &machine->instrArray[targetInstrAddress];
      if(instr->opcode != Opcode_LABEL)
        break;
    }
    if(targetInstrAddress < machine->instrCount)
    {
      Instruction* labelInstr = &machine->instrArray[label->instrAddress];
      labelInstr->paramType = Param_Int32;
      labelInstr->iParam = targetInstrAddress;
      label->instrAddress = targetInstrAddress;
    }
    else {
      Error("Could not find a non-label instruction following the label %s", label->string);
      return false;
    }
  }

  for(int instrAddress = 0; instrAddress < machine->instrCount; instrAddress++)
  {
    Instruction* instr = &machine->instrArray[instrAddress];
    if(instr->opcode == Opcode_GOTO ||
       instr->opcode == Opcode_JUMPNZ ||
       instr->opcode == Opcode_JUMPZ ||
       instr->opcode == Opcode_CALL)
    {
      assert(instr->paramType == Param_String);
      Label* label = FindLabel(source, instr->strParam);
      if(label)
      {
        instr->paramType = Param_Int32;
        instr->iParam = label->instrAddress;
      }
      else {
        Error("Label '%s' not found", instr->strParam);
        return false;
      }
    }
  }

  return true;
}/*<<<*/

bool32 CheckStackBounds(Machine* machine, int sp)
{
  return sp >= 0 && sp*VMWORD < machine->memorySize;
}

bool32 CheckMemoryBounds(Machine* machine, int location)
{
  return location >= 0 && location < machine->memorySize;
}

bool32 CheckInstructionBounds(Machine* machine, int address)
{
  return address >= 0 && address < machine->instrCount;
}

ExecResult ExecuteInstruction(Machine* machine, Instruction* instr)
{/*>>>*/
  uint8* memory = machine->memory;
  Opcode opcode = instr->opcode;

  switch(opcode)
  {
    case Opcode_ALLOC:
      {
        assert(instr->paramType == Param_Int32);
        int32 topSp = machine->sp+instr->iParam;
        if(CheckStackBounds(machine, topSp))
        {
          for(int i = machine->sp; i < topSp; i++)
            *((int32*)memory+i) = 0;
          machine->sp = topSp;
          machine->ip++;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_PUSH:
      {
        int32 topSp = machine->sp+1;
        if(CheckStackBounds(machine, topSp))
        {
          if(instr->paramType == Param_Int32)
            *(int32*)&memory[machine->sp*VMWORD] = instr->iParam;
          else if(instr->paramType == Param_String)
          {
            char* registerName = instr->strParam;
            if(StrMatch(registerName, "sp"))
              *(int32*)&memory[machine->sp*VMWORD] = machine->sp;
            else if(StrMatch(registerName, "ip"))
              *(int32*)&memory[machine->sp*VMWORD] = machine->ip;
            else if(StrMatch(registerName, "fp"))
              *(int32*)&memory[machine->sp*VMWORD] = machine->fp;
            else
              assert(false);
          } else
            assert(false);

          machine->sp = topSp;
          machine->ip++;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_POP:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          if(instr->paramType == Param__Null)
          {
            machine->sp--;
            machine->ip++;
          }
          else if(instr->paramType == Param_Int32)
          {
            machine->sp += instr->iParam;
            machine->ip++;
          }
          else if(instr->paramType == Param_String)
          {
            char* registerName = instr->strParam;
            if(StrMatch(registerName, "sp"))
            {
              machine->sp = *(int32*)&memory[argSp*VMWORD];
              machine->ip++;
            }
            else if(StrMatch(registerName, "ip"))
            {
              int32 ip = *(int32*)&memory[argSp*VMWORD];
              machine->sp--;
              machine->ip = ip;
            }
            else if(StrMatch(registerName, "fp"))
            {
              machine->fp = *(int32*)&memory[argSp*VMWORD];
              machine->sp--;
              machine->ip++;
            } else
              assert(false);
          } else
            assert(false);
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_ADD:
    case Opcode_DIV:
    case Opcode_MUL:
    case Opcode_SUB:
      {
        int32 argSp = machine->sp-2;
        if(CheckStackBounds(machine, argSp))
        {
          int32 arg1 = *(int32*)&memory[argSp*VMWORD];
          int32 arg2 = *(int32*)&memory[(argSp+1)*VMWORD];
          int32 result = 0;
          switch(opcode)
          {
            case Opcode_ADD:
              result = arg1+arg2;
              break;
            case Opcode_SUB:
              result = arg1-arg2;
              break;
            case Opcode_MUL:
              result = arg1*arg2;
              break;
            case Opcode_DIV:
              if(arg2 != 0)
                result = arg1/arg2;
              else
                return ExecResult_DivByZero;
              break;
          }
          *(int32*)&memory[argSp*VMWORD] = result;
          machine->sp--;
          machine->ip++;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_INC:
    case Opcode_DEC:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 arg = *(int32*)&memory[argSp*VMWORD];
          int32 result = arg;
          if(opcode == Opcode_INC)
            result++;
          else
            result--;
          *(int32*)&memory[argSp*VMWORD] = result;
          machine->ip++;
        }
      } break;

    case Opcode_LOAD:
    case Opcode_LOAD8:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 location = *(int32*)&memory[argSp*VMWORD];
          if(CheckMemoryBounds(machine, location))
          {
            if(opcode == Opcode_LOAD)
            {
              uint32 value = *(uint32*)&memory[location*VMWORD];
              *(uint32*)&memory[argSp*VMWORD] = (uint32)value;
            }
            else if(opcode == Opcode_LOAD8)
            {
              uint8 value = *(uint8*)&memory[location];
              *(uint32*)&memory[argSp] = (uint32)value;
            }
            else
              assert(false);

            machine->ip++;
          } else
            return ExecResult_InvalidMemoryAccess;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_STORE:
    case Opcode_STORE8:
      {
        int32 locationArgSp = machine->sp-1;
        if(CheckStackBounds(machine, locationArgSp))
        {
          int32 location = *(int32*)&memory[locationArgSp*VMWORD];
          if(CheckMemoryBounds(machine, location))
          {
            int32 valueArgSp = machine->sp-2;
            if(CheckStackBounds(machine, valueArgSp))
            {
              uint32 value = *(uint32*)&memory[valueArgSp*VMWORD];
              if(opcode == Opcode_STORE)
              {
                if(value <= 0xffffffff)
                  *((uint32*)&memory[location*VMWORD]) = (uint32)value;
                else
                  return ExecResult_InvalidOperandSize;
              }
              else if(opcode == Opcode_STORE8)
              {
                if(value <= 0xff)
                  *((uint8*)&memory[location]) = (uint8)value;
                else
                  return ExecResult_InvalidOperandSize;
              }
              else
                assert(false);

              machine->sp--;
              machine->ip++;
            } else
              return ExecResult_InvalidMemoryAccess;
          } else
            return ExecResult_InvalidMemoryAccess;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_GOTO:
      {
        assert(instr->paramType == Param_Int32);
        machine->ip = instr->iParam;
      } break;

    case Opcode_CALL:
      {
        assert(instr->paramType == Param_Int32);
        int32 topSp = machine->sp+3;
        if(CheckStackBounds(machine, topSp))
        {
          *(int32*)&memory[machine->sp*VMWORD] = machine->ip+1;
          *(int32*)&memory[(machine->sp+1)*VMWORD] = machine->fp;
          *(int32*)&memory[(machine->sp+2)*VMWORD] = machine->sp;

          int32 jumpAddress = instr->iParam;
          machine->ip = jumpAddress;
          machine->sp = topSp;
          machine->fp = machine->sp;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_RETURN:
      {
        int32 argSp = machine->fp-3;
        if(CheckStackBounds(machine, argSp))
        {
          machine->ip = *(int32*)&memory[argSp*VMWORD];
          machine->fp = *(int32*)&memory[(argSp+1)*VMWORD];
          machine->sp = *(int32*)&memory[(argSp+2)*VMWORD];
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_JUMPNZ:
    case Opcode_JUMPZ:
      {
        assert(instr->paramType == Param_Int32);
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 jumpAddress = instr->iParam;
          int32 check = *(int32*)&memory[argSp*VMWORD];
          if((check && opcode == Opcode_JUMPNZ) ||
              (!check && opcode == Opcode_JUMPZ))
          {
            machine->ip = jumpAddress;
          }
          else
            machine->ip++;
          machine->sp--;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_DUP:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          uint32 value = *(uint32*)&memory[argSp*VMWORD];
          if(CheckStackBounds(machine, machine->sp))
          {
            *(uint32*)&memory[machine->sp] = value;
            machine->sp++;
            machine->ip++;
          } else
            return ExecResult_InvalidMemoryAccess;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

//    case Opcode_PRINT:
//      {/*>>>*/
//        int32 sp = machine->sp-VMWORD;
//        if(CheckStackBounds(machine, sp))
//        {
//          int32 location = *(int32*)&memory[sp];
//          for(char* charPtr = (char*)(memory + location);
//              *charPtr != '\0';
//              charPtr++)
//          {
//            putchar(*charPtr);
//          }
//          machine->sp = sp;
//          machine->ip++;
//        } else
//          return ExecResult_InvalidMemoryAccess;
//      } break;/*<<<*/

    case Opcode_HALT:
        return ExecResult_EndOfProgram;

    case Opcode_LABEL:
      {
        assert(instr->paramType == Param_Int32);
        int32 jumpAddress = instr->iParam;
        machine->ip = jumpAddress;
      } break;

    case Opcode_NOOP:
      {
        machine->ip++;
      } break;

    default:
        return ExecResult_IllegalInstruction;
  }

  return ExecResult_OK;
}/*<<<*/

ExecResult RunProgram(Machine* machine)
{
  Instruction* instrArray = machine->instrArray;

  Instruction* instr;
  ExecResult execResult = ExecResult_OK;
  do
  {
    int ip = machine->ip;
    if(CheckInstructionBounds(machine, ip))
    {
      instr = &instrArray[ip];
      execResult = ExecuteInstruction(machine, instr);
    }
    else
      execResult = ExecResult_InvalidInstructionAddress;
  }
  while(execResult == ExecResult_OK);

  if(execResult == ExecResult_EndOfProgram)
  {
    //Memory dump
    for(int i = 0; i <= VMWORD*30; i += VMWORD)
      printf("%d ", *(int32*)&machine->memory[i]);
  }
  else {
    switch(execResult)
    {
      case ExecResult_InvalidMemoryAccess:
        Error("Invalid memory location");
        break;
      case ExecResult_InvalidInstructionAddress:
        Error("Invalid instruction address");
        break;
      case ExecResult_IllegalInstruction:
        Error("Illegal instruction");
        break;
      case ExecResult_InvalidOperandSize:
        Error("Invalid operand size");
        break;
      case ExecResult_DivByZero:
        Error("Division by zero");
        break;

      default:
        assert(!"Please implement me!");
    }
  }

  return execResult;
}

char* ReadIrText(MemoryArena* arena)
{
  char* text = 0;
  HRSRC res = FindResource(0, "CODE", "VM");
  if(res)
  {
    HGLOBAL resData = LoadResource(0, res);
    int size = SizeofResource(0, res);
    char* data = (char*)LockResource(resData);
    text = PushElement(arena, char, size+1); // 0-terminator
    char* pChar = text;
    for(int i = 0; i < size; i++)
      *pChar++ = *data++;
    *pChar = '\0';
  } else
    Error("IR code not found");
  return text;
}

int main(int argc, char* argv[])
{
  MemoryArena arena = NewArena(10*MEGABYTE);

  Machine machine = {};
  machine.instrArraySize = SizeofArray(machine.instrArray);

  char* text = ReadIrText(&arena);
  if(text)
  {
    SourceProgram source = {};
    source.text = text;
    ProcessSourceLines(&source);
    machine.source = &source;

    if(BuildInstructionArray(&machine))
    {
      machine.ip = 0;
      machine.sp = 0;
      machine.memorySize = SizeofArray(machine.memory);

      RunProgram(&machine);
    } else {
      Error("Instruction array could not be built");
    }
  } else {
    Error("Size of program text is out of range");
  }

  return 0;
}

