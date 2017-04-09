#include "lib.c"
#include "hasm.h"

#define VMWORD (sizeof(int32))

typedef struct
{/*>>>*/
  HasmCode* code;
  int32 mem_size;
  int32 fp;
  int32 sp;
  int32 ip;
  uint8 memory[2048];
}/*<<<*/
HocMachine;

typedef enum
{/*>>>*/
  ExecResult_EndOfProgram = 0,
  ExecResult_OK,
  ExecResult_InvalidMemoryAccess,
  ExecResult_InvalidInstructionAddress,
  ExecResult_InvalidOperandSize,
  ExecResult_IllegalInstruction,
  ExecResult_DivByZero,

  ExecResult__Count
}/*<<<*/
ExecResult;

bool32
check_stack_bounds(HocMachine* machine, int sp)
{/*>>>*/
  return sp >= 0 && sp*(int)VMWORD < machine->mem_size;
}/*<<<*/

bool32
check_memory_bounds(HocMachine* machine, int location)
{/*>>>*/
  return location >= 0 && location < machine->mem_size;
}/*<<<*/

bool32
check_instr_bounds(HasmCode* code, int address)
{/*>>>*/
  return address >= 0 && address < code->instr_count;
}/*<<<*/

ExecResult
execute_instr(HocMachine* machine, Instruction* instr)
{/*>>>*/
  uint8* memory = machine->memory;
  Opcode opcode = instr->opcode;

  switch(opcode)
  {
    case Opcode_ALLOC:
    {/*>>>*/
      assert(instr->param_type == ParamType_Int32);
      int32 top_sp = machine->sp + instr->param.int_num;
      if(check_stack_bounds(machine, top_sp))
      {
        for(int i = machine->sp; i < top_sp; i++)
          *((int32*)memory+i) = 0;
        machine->sp = top_sp;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_PUSH:
    {/*>>>*/
      int32 top_sp = machine->sp+1;
      if(check_stack_bounds(machine, top_sp))
      {
        if(instr->param_type == ParamType_Int32)
          *(int32*)&memory[machine->sp*VMWORD] = instr->param.int_num;
        else if(instr->param_type == ParamType_Reg)
        {
          RegName regname = instr->param.reg;
          if(regname == RegName_SP)
            *(int32*)&memory[machine->sp*VMWORD] = machine->sp;
          else if(regname == RegName_IP)
            *(int32*)&memory[machine->sp*VMWORD] = machine->ip;
          else if(regname == RegName_FP)
            *(int32*)&memory[machine->sp*VMWORD] = machine->fp;
          else
            assert(false);
        } else
          assert(false);

        machine->sp = top_sp;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_POP:
    {/*>>>*/
      int32 arg_sp = machine->sp-1;
      if(check_stack_bounds(machine, arg_sp))
      {
        if(instr->param_type == ParamType__Null)
        {
          machine->sp--;
          machine->ip++;
        }
        else if(instr->param_type == ParamType_Int32)
        {
          machine->sp -= instr->param.int_num;
          machine->ip++;
        }
        else if(instr->param_type == ParamType_Reg)
        {
          RegName regname = instr->param.reg;
          if(regname == RegName_SP)
          {
            machine->sp = *(int32*)&memory[arg_sp*VMWORD];
            machine->ip++;
          }
          else if(regname == RegName_IP)
          {
            int32 ip = *(int32*)&memory[arg_sp*VMWORD];
            machine->sp--;
            machine->ip = ip;
          }
          else if(regname == RegName_FP)
          {
            machine->fp = *(int32*)&memory[arg_sp*VMWORD];
            machine->sp--;
            machine->ip++;
          } else
            assert(false);
        } else
          assert(false);
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_ADD:
    case Opcode_DIV:
    case Opcode_MUL:
    case Opcode_SUB:
    {/*>>>*/
      int32 arg_sp = machine->sp-2;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 arg1 = *(int32*)&memory[arg_sp*VMWORD];
        int32 arg2 = *(int32*)&memory[(arg_sp+1)*VMWORD];

        int32 result = 0;
        if(opcode == Opcode_ADD) {
          result = arg1 + arg2;
        }
        else if(opcode == Opcode_SUB) {
          result = arg1 - arg2;
        }
        else if(opcode == Opcode_MUL) {
          result = arg1 * arg2;
        }
        else if(opcode == Opcode_DIV)
        {
          if(arg2 != 0)
            result = arg1 / arg2;
          else
            return ExecResult_DivByZero;
        }
        else
          assert(false);

        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->sp--;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_AND:
    case Opcode_OR:
    {/*>>>*/
      int32 arg_sp = machine->sp-2;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 arg1 = *(int32*)&memory[arg_sp*VMWORD];
        int32 arg2 = *(int32*)&memory[(arg_sp+1)*VMWORD];

        bool32 result = 0;
        if(opcode == Opcode_AND) {
          result = arg1 && arg2;
        }
        else if(opcode == Opcode_OR) {
          result = arg1 || arg2;
        }
        else
          assert(false);

        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->sp--;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_NOT:
    {/*>>>*/
      int32 arg_sp = machine->sp-1;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 arg = *(int32*)&memory[arg_sp*VMWORD];
        bool32 result = !arg;
        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_INCR:
    case Opcode_DECR:
    {/*>>>*/
      int32 arg_sp = machine->sp-1;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 arg = *(int32*)&memory[arg_sp*VMWORD];
        int32 result = arg;
        if(opcode == Opcode_INCR)
          result++;
        else
          result--;
        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->ip++;
      }
    } break;/*<<<*/

    case Opcode_LOAD:
    case Opcode_LOAD8:
    {/*>>>*/
      int32 arg_sp = machine->sp-1;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 location = *(int32*)&memory[arg_sp*VMWORD];
        if(check_memory_bounds(machine, location))
        {
          if(opcode == Opcode_LOAD)
          {
            uint32 value = *(uint32*)&memory[location*VMWORD];
            *(uint32*)&memory[arg_sp*VMWORD] = (uint32)value;
          }
          else if(opcode == Opcode_LOAD8)
          {
            uint8 value = *(uint8*)&memory[location];
            *(uint32*)&memory[arg_sp] = (uint32)value;
          }
          else
            assert(false);

          machine->ip++;
        } else
          return ExecResult_InvalidMemoryAccess;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_STORE:
    case Opcode_STORE8:
    {/*>>>*/
      int32 loc_arg_sp = machine->sp-1;
      int32 val_arg_sp = machine->sp-2;
      if(check_stack_bounds(machine, val_arg_sp))
      {
        int32 location = *(int32*)&memory[loc_arg_sp*VMWORD];
        if(check_memory_bounds(machine, location))
        {
          uint32 value = *(uint32*)&memory[val_arg_sp*VMWORD];
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
    } break;/*<<<*/

    case Opcode_DUP:
    {/*>>>*/
      int32 arg_sp = machine->sp-1;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 value = *(int32*)&memory[arg_sp*VMWORD];
        if(check_stack_bounds(machine, machine->sp))
        {
          *(int32*)&memory[machine->sp*VMWORD] = value;
          machine->sp++;
          machine->ip++;
        } else
          return ExecResult_InvalidMemoryAccess;
      } else
        return ExecResult_InvalidMemoryAccess;
      } break;/*<<<*/

    case Opcode_GOTO:
    {/*>>>*/
      assert(instr->param_type == ParamType_Int32);
      machine->ip = instr->param.int_num;
    } break;/*<<<*/

    case Opcode_CALL:
    {/*>>>*/
      assert(instr->param_type == ParamType_Int32);
      int32 top_sp = machine->sp+3;
      if(check_stack_bounds(machine, top_sp))
      {
        *(int32*)&memory[machine->sp*VMWORD] = machine->ip+1;
        *(int32*)&memory[(machine->sp+1)*VMWORD] = machine->fp;
        *(int32*)&memory[(machine->sp+2)*VMWORD] = machine->sp;

        int32 jump_address = instr->param.int_num;
        machine->ip = jump_address;
        machine->sp = top_sp;
        machine->fp = top_sp;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_RETURN:
    {/*>>>*/
      int32 arg_sp = machine->fp-3;
      if(check_stack_bounds(machine, arg_sp))
      {
        machine->ip = *(int32*)&memory[arg_sp*VMWORD];
        machine->fp = *(int32*)&memory[(arg_sp+1)*VMWORD];
        machine->sp = *(int32*)&memory[(arg_sp+2)*VMWORD];
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_ENTER:
    {/*>>>*/
      int32 top_sp = machine->sp+1;
      if(check_stack_bounds(machine, top_sp))
      {
        *(int32*)&memory[machine->sp*VMWORD] = machine->fp;
        machine->fp = top_sp;
        machine->sp = top_sp;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_LEAVE:
    {/*>>>*/
      int32 arg_sp = machine->fp-1;
      if(check_stack_bounds(machine, arg_sp))
      {
        machine->fp = *(int32*)&memory[arg_sp*VMWORD];
        machine->sp = arg_sp;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_JUMPZ:
    case Opcode_JUMPNZ:
    {/*>>>*/
      assert(instr->param_type == ParamType_Int32);
      int32 arg_sp = machine->sp-1;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 jump_address = instr->param.int_num;
        int32 check = *(int32*)&memory[arg_sp*VMWORD];
        if((check && opcode == Opcode_JUMPNZ) ||
           (!check && opcode == Opcode_JUMPZ))
        {
          machine->ip = jump_address;
        }
        else
          machine->ip++;
        machine->sp--;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_CMPEQ:
    case Opcode_CMPNEQ:
    {/*>>>*/
      int32 arg_sp = machine->sp-2;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 arg1 = *(int32*)&memory[arg_sp*VMWORD];
        int32 arg2 = *(int32*)&memory[(arg_sp+1)*VMWORD];
        int32 result = (arg1 == arg2);
        if(opcode == Opcode_CMPNEQ)
          result = !result;
        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->sp--;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

    case Opcode_CMPLSS:
    case Opcode_CMPGRT:
    {/*>>>*/
      int32 arg_sp = machine->sp-2;
      if(check_stack_bounds(machine, arg_sp))
      {
        int32 arg1 = *(int32*)&memory[arg_sp*VMWORD];
        int32 arg2 = *(int32*)&memory[(arg_sp+1)*VMWORD];
        int32 result = (arg1 < arg2);
        if(opcode == Opcode_CMPGRT)
          result = (arg1 > arg2);
        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->sp--;
        machine->ip++;
      } else
        return ExecResult_InvalidMemoryAccess;
    } break;/*<<<*/

//    case Opcode_PRINT:
//      {/*>>>*/
//        int32 sp = machine->sp-VMWORD;
//        if(check_stack_bounds(machine, sp))
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
    {/*>>>*/
      assert(instr->param_type == ParamType_Int32);
      int32 jump_address = instr->param.int_num;
      machine->ip = jump_address;
    } break;/*<<<*/

    case Opcode_NOOP:
    {/*>>>*/
      machine->ip++;
    } break;/*<<<*/

    default:
        return ExecResult_IllegalInstruction;
  }

  return ExecResult_OK;
}/*<<<*/

ExecResult
run_program(HocMachine* machine)
{/*>>>*/
  HasmCode* code = machine->code;

  Instruction* instr;
  ExecResult exec_result = ExecResult_OK;
  do
  {
    int ip = machine->ip;
    if(check_instr_bounds(code, ip))
    {
      instr = &code->instr_array[ip];
      exec_result = execute_instr(machine, instr);
    }
    else
      exec_result = ExecResult_InvalidInstructionAddress;
  }
  while(exec_result == ExecResult_OK);

  if(exec_result == ExecResult_EndOfProgram)
  {
    //Memory dump
    for(int i = 0; i <= VMWORD*30; i += VMWORD)
      printf("%d ", *(int32*)&machine->memory[i]);
  }
  else {
    switch(exec_result)
    {
      case ExecResult_InvalidMemoryAccess:
        error("Access to invalid memory location");
        break;
      case ExecResult_InvalidInstructionAddress:
        error("Invalid instruction address");
        break;
      case ExecResult_IllegalInstruction:
        error("Illegal instruction");
        break;
      case ExecResult_InvalidOperandSize:
        error("Invalid operand size");
        break;
      case ExecResult_DivByZero:
        error("Division by zero");
        break;

      default:
        assert(!"Please implement me!");
    }
  }

  return exec_result;
}/*<<<*/

bool32
load_ir_code(HasmCode* code)
{/*>>>*/
  HRSRC res = FindResource(0, "CODE", "VM");
  if(res)
  {
    HGLOBAL res_data = LoadResource(0, res);
    HasmCode* res_code = (HasmCode*)LockResource(res_data);
    if(cstr_match(res_code->groove, "IRC"))
    {
      // Fix the pointers
      *code = *res_code;
      code->code_start = (uint8*)res_code;
      code->instr_array = (Instruction*)(code->code_start + sizeof(HasmCode));
      return true;
    } else
      error("Resource does not appear to be valid IR code");
  } else
    error("IR code not found");
  return false;
}/*<<<*/

int
main(int argc, char* argv[])
{/*>>>*/
  int ret = -1;

  HocMachine machine = {0};
  HasmCode code = {0};
  if(load_ir_code(&code))
  {
    machine.mem_size = sizeof_array(machine.memory);
    machine.code = &code;

    ret = (int)run_program(&machine);
  }
  return ret;
}/*<<<*/

