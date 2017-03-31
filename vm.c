#include "lib.c"
#include "ir.h"

#define VMWORD (sizeof(int32))

typedef struct
{
  IrCode* code;
  int32 memorySize;
  int32 fp;
  int32 sp;
  int32 ip;
  uint8 memory[2048];
}
Machine;

typedef enum
{
  ExecResult_EndOfProgram = 0,
  ExecResult_OK,
  ExecResult_InvalidMemoryAccess,
  ExecResult_InvalidInstructionAddress,
  ExecResult_InvalidOperandSize,
  ExecResult_IllegalInstruction,
  ExecResult_DivByZero,

  ExecResult__Count
}
ExecResult;

bool32 CheckStackBounds(Machine* machine, int sp)
{
  return sp >= 0 && sp*(int)VMWORD < machine->memorySize;
}

bool32 CheckMemoryBounds(Machine* machine, int location)
{
  return location >= 0 && location < machine->memorySize;
}

bool32 CheckInstructionBounds(IrCode* code, int address)
{
  return address >= 0 && address < code->instrCount;
}

ExecResult ExecuteInstruction(Machine* machine, Instruction* instr)
{/*>>>*/
  uint8* memory = machine->memory;
  Opcode opcode = instr->opcode;

  switch(opcode)
  {
    case Opcode_ALLOC:
      {
        assert(instr->paramType == ParamType_Int32);
        int32 topSp = machine->sp + instr->param.intNum;
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
          if(instr->paramType == ParamType_Int32)
            *(int32*)&memory[machine->sp*VMWORD] = instr->param.intNum;
          else if(instr->paramType == ParamType_Reg)
          {
            RegName regName = instr->param.reg;
            if(regName == RegName_SP)
              *(int32*)&memory[machine->sp*VMWORD] = machine->sp;
            else if(regName == RegName_IP)
              *(int32*)&memory[machine->sp*VMWORD] = machine->ip;
            else if(regName == RegName_FP)
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
          if(instr->paramType == ParamType__Null)
          {
            machine->sp--;
            machine->ip++;
          }
          else if(instr->paramType == ParamType_Int32)
          {
            machine->sp -= instr->param.intNum;
            machine->ip++;
          }
          else if(instr->paramType == ParamType_Reg)
          {
            RegName regName = instr->param.reg;
            if(regName == RegName_SP)
            {
              machine->sp = *(int32*)&memory[argSp*VMWORD];
              machine->ip++;
            }
            else if(regName == RegName_IP)
            {
              int32 ip = *(int32*)&memory[argSp*VMWORD];
              machine->sp--;
              machine->ip = ip;
            }
            else if(regName == RegName_FP)
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

    case Opcode_INCR:
    case Opcode_DECR:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 arg = *(int32*)&memory[argSp*VMWORD];
          int32 result = arg;
          if(opcode == Opcode_INCR)
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
        int32 locationArgSp = machine->sp-2;
        if(CheckStackBounds(machine, locationArgSp))
        {
          int32 location = *(int32*)&memory[locationArgSp*VMWORD];
          if(CheckMemoryBounds(machine, location))
          {
            int32 valueArgSp = machine->sp-1;
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

            *(int32*)&memory[locationArgSp*VMWORD] = value;
            machine->sp--;
            machine->ip++;
          } else
            return ExecResult_InvalidMemoryAccess;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_GOTO:
      {
        assert(instr->paramType == ParamType_Int32);
        machine->ip = instr->param.intNum;
      } break;

    case Opcode_CALL:
      {
        assert(instr->paramType == ParamType_Int32);
        int32 topSp = machine->sp+3;
        if(CheckStackBounds(machine, topSp))
        {
          *(int32*)&memory[machine->sp*VMWORD] = machine->ip+1;
          *(int32*)&memory[(machine->sp+1)*VMWORD] = machine->fp;
          *(int32*)&memory[(machine->sp+2)*VMWORD] = machine->sp;

          int32 jumpAddress = instr->param.intNum;
          machine->ip = jumpAddress;
          machine->sp = topSp;
          machine->fp = topSp;
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

    case Opcode_ENTER:
      {
        int32 topSp = machine->sp+1;
        if(CheckStackBounds(machine, topSp))
        {
          *(int32*)&memory[machine->sp*VMWORD] = machine->fp;
          machine->fp = topSp;
          machine->sp = topSp;
          machine->ip++;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_LEAVE:
      {
        int32 argSp = machine->fp-1;
        if(CheckStackBounds(machine, argSp))
        {
          machine->fp = *(int32*)&memory[argSp*VMWORD];
          machine->sp = argSp;
          machine->ip++;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_JUMPNZ:
    case Opcode_JUMPZ:
      {
        assert(instr->paramType == ParamType_Int32);
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 jumpAddress = instr->param.intNum;
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
          int32 value = *(int32*)&memory[argSp*VMWORD];
          if(CheckStackBounds(machine, machine->sp))
          {
            *(int32*)&memory[machine->sp*VMWORD] = value;
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
        assert(instr->paramType == ParamType_Int32);
        int32 jumpAddress = instr->param.intNum;
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
{/*>>>*/
  IrCode* code = machine->code;

  Instruction* instr;
  ExecResult execResult = ExecResult_OK;
  do
  {
    int ip = machine->ip;
    if(CheckInstructionBounds(code, ip))
    {
      instr = &code->instrArray[ip];
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
        Error("Access to invalid memory location");
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
}/*<<<*/

bool32 LoadIrCode(IrCode* code)
{
  HRSRC res = FindResource(0, "CODE", "VM");
  if(res)
  {
    HGLOBAL resData = LoadResource(0, res);
    IrCode* resCode = (IrCode*)LockResource(resData);
    if(StrMatch(resCode->groove, "IRC"))
    {
      // Fix the pointers
      *code = *resCode;
      code->codeStart = (uint8*)resCode;
      code->instrArray = (Instruction*)(code->codeStart + sizeof(IrCode));
      return true;
    } else
      Error("Resource does not appear to be valid IR code");
  } else
    Error("IR code not found");
  return false;
}

int main(int argc, char* argv[])
{
  int ret = -1;

  Machine machine = {0};
  IrCode code = {0};
  if(LoadIrCode(&code))
  {
    machine.memorySize = SizeofArray(machine.memory);
    machine.code = &code;

    ret = (int)RunProgram(&machine);
  }
  return ret;
}
