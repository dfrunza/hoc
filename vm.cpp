#include "lib.cpp"
#include "ir.h"

#define VMWORD (sizeof(int32))

struct Machine
{
  IrCode* code;
  int32 memorySize;
  int32 fp;
  int32 sp;
  int32 ip;
  uint8 memory[2048];
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
    case Opcode::ALLOC:
      {
        assert(instr->paramType == ParamType::Int32);
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

    case Opcode::PUSH:
      {
        int32 topSp = machine->sp+1;
        if(CheckStackBounds(machine, topSp))
        {
          if(instr->paramType == ParamType::Int32)
            *(int32*)&memory[machine->sp*VMWORD] = instr->param.intNum;
          else if(instr->paramType == ParamType::Reg)
          {
            RegName regName = instr->param.reg;
            if(regName == RegName::SP)
              *(int32*)&memory[machine->sp*VMWORD] = machine->sp;
            else if(regName == RegName::IP)
              *(int32*)&memory[machine->sp*VMWORD] = machine->ip;
            else if(regName == RegName::FP)
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

    case Opcode::POP:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          if(instr->paramType == ParamType::_Null)
          {
            machine->sp--;
            machine->ip++;
          }
          else if(instr->paramType == ParamType::Int32)
          {
            machine->sp -= instr->param.intNum;
            machine->ip++;
          }
          else if(instr->paramType == ParamType::Reg)
          {
            RegName regName = instr->param.reg;
            if(regName == RegName::SP)
            {
              machine->sp = *(int32*)&memory[argSp*VMWORD];
              machine->ip++;
            }
            else if(regName == RegName::IP)
            {
              int32 ip = *(int32*)&memory[argSp*VMWORD];
              machine->sp--;
              machine->ip = ip;
            }
            else if(regName == RegName::FP)
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

    case Opcode::ADD:
    case Opcode::DIV:
    case Opcode::MUL:
    case Opcode::SUB:
      {
        int32 argSp = machine->sp-2;
        if(CheckStackBounds(machine, argSp))
        {
          int32 arg1 = *(int32*)&memory[argSp*VMWORD];
          int32 arg2 = *(int32*)&memory[(argSp+1)*VMWORD];
          int32 result = 0;
          switch(opcode)
          {
            case Opcode::ADD:
              result = arg1+arg2;
              break;
            case Opcode::SUB:
              result = arg1-arg2;
              break;
            case Opcode::MUL:
              result = arg1*arg2;
              break;
            case Opcode::DIV:
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

    case Opcode::INCR:
    case Opcode::DECR:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 arg = *(int32*)&memory[argSp*VMWORD];
          int32 result = arg;
          if(opcode == Opcode::INCR)
            result++;
          else
            result--;
          *(int32*)&memory[argSp*VMWORD] = result;
          machine->ip++;
        }
      } break;

    case Opcode::LOAD:
    case Opcode::LOAD8:
      {
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 location = *(int32*)&memory[argSp*VMWORD];
          if(CheckMemoryBounds(machine, location))
          {
            if(opcode == Opcode::LOAD)
            {
              uint32 value = *(uint32*)&memory[location*VMWORD];
              *(uint32*)&memory[argSp*VMWORD] = (uint32)value;
            }
            else if(opcode == Opcode::LOAD8)
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

    case Opcode::STORE:
    case Opcode::STORE8:
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
              if(opcode == Opcode::STORE)
              {
                if(value <= 0xffffffff)
                  *((uint32*)&memory[location*VMWORD]) = (uint32)value;
                else
                  return ExecResult_InvalidOperandSize;
              }
              else if(opcode == Opcode::STORE8)
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

    case Opcode::GOTO:
      {
        assert(instr->paramType == ParamType::Int32);
        machine->ip = instr->param.intNum;
      } break;

    case Opcode::CALL:
      {
        assert(instr->paramType == ParamType::Int32);
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

    case Opcode::RETURN:
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

    case Opcode::ENTER:
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

    case Opcode::LEAVE:
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

    case Opcode::JUMPNZ:
    case Opcode::JUMPZ:
      {
        assert(instr->paramType == ParamType::Int32);
        int32 argSp = machine->sp-1;
        if(CheckStackBounds(machine, argSp))
        {
          int32 jumpAddress = instr->param.intNum;
          int32 check = *(int32*)&memory[argSp*VMWORD];
          if((check && opcode == Opcode::JUMPNZ) ||
              (!check && opcode == Opcode::JUMPZ))
          {
            machine->ip = jumpAddress;
          }
          else
            machine->ip++;
          machine->sp--;
        } else
          return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode::DUP:
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

//    case Opcode::PRINT:
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

    case Opcode::HALT:
        return ExecResult_EndOfProgram;

    case Opcode::LABEL:
      {
        assert(instr->paramType == ParamType::Int32);
        int32 jumpAddress = instr->param.intNum;
        machine->ip = jumpAddress;
      } break;

    case Opcode::NOOP:
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

  Machine machine = {};
  IrCode code = {};
  if(LoadIrCode(&code))
  {
    machine.memorySize = SizeofArray(machine.memory);
    machine.code = &code;

    ret = (int)RunProgram(&machine);
  }
  return ret;
}
