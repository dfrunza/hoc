#include "hocc.h"

#define VMWORD (sizeof(int32))
#define VM_MEMORY_SIZE 1024

typedef struct
{
  BinCode* code;
  int32 fp;
  int32 sp;
  int32 hp;
  int32 ip;
  int32 memory_size;
  uint8* memory;
}
HocMachine;

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

local MemoryArena* arena;

local bool32
check_sp_bounds(HocMachine* machine, int sp)
{
  return sp >= 0 && sp*(int)VMWORD < machine->hp;
}

local bool32
check_memory_bounds(HocMachine* machine, int location)
{
  return location > 0 && location <= machine->memory_size;
}

local bool32
check_hp_bounds(HocMachine* machine, int hp)
{
  return hp > machine->sp && hp < machine->memory_size;
}

local bool32
check_instr_bounds(BinCode* code, int address)
{
  return address >= 0 && address < code->instr_count;
}

void
clear_memory(uint8* memory, int base, int word_count)
{
  for(int i = 0; i < word_count; i++)
    ((int32*)memory)[base + i] = 0xcdcdcdcd;
}

local ExecResult
execute_instr(HocMachine* machine, Instruction* instr)
{
  uint8* memory = machine->memory;
  Opcode opcode = instr->opcode;

  switch(opcode)
  {
    case Opcode_ALLOC:
    {
      assert(instr->param_type == ParamType_Int32);
      int32 top_sp = machine->sp + instr->param.int_val;
      if(check_sp_bounds(machine, top_sp))
      {
#if 0
        for(int i = 0; i < instr->param.int_val; i++)
          ((int32*)memory)[machine->sp + i] = 0xcdcdcdcd;
#else
        clear_memory(memory, machine->sp, instr->param.int_val);
#endif

        machine->sp = top_sp;
        machine->ip++;
      } 
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_NEW:
    {
      assert(instr->param_type == ParamType_Int32);
      int32 top_hp = machine->hp - instr->param.int_val;
      int32 top_sp = machine->sp+1;
      if((top_hp < machine->hp) && check_hp_bounds(machine, top_hp) && check_sp_bounds(machine, top_sp))
      {
#if 0
        for(int i = 0; i < instr->param.int_val; i++)
          ((int32*)memory)[top_hp + i] = 0xcdcdcdcd;
#else
        clear_memory(memory, top_hp, instr->param.int_val);
#endif

        *(int32*)&memory[machine->sp*VMWORD] = top_hp;

        machine->hp = top_hp;
        machine->sp++;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_PUSH:
    case Opcode_PUSHF:
    {
      int32 top_sp = machine->sp+1;
      if(check_sp_bounds(machine, top_sp))
      {
        if(opcode == Opcode_PUSH)
        {
          if(instr->param_type == ParamType_Int32)
            *(int32*)&memory[machine->sp*VMWORD] = instr->param.int_val;
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
              assert(0);
          }
          else
            assert(0);
        }
        else if(opcode == Opcode_PUSHF)
        {
          if(instr->param_type == ParamType_Float32)
            *(float32*)&memory[machine->sp*VMWORD] = instr->param.float_val;
          else
            assert(0);
        }

        machine->sp = top_sp;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_POP:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        if(instr->param_type == ParamType__Null)
        {
          machine->sp--;
          machine->ip++;
        }
        else if(instr->param_type == ParamType_Int32)
        {
          machine->sp -= instr->param.int_val;
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
          }
          else
            assert(0);
        }
        else
          assert(0);
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_ADD:
    case Opcode_SUB:
    case Opcode_MUL:
    case Opcode_DIV:
    case Opcode_MOD:
    case Opcode_ADDF:
    case Opcode_SUBF:
    case Opcode_MULF:
    case Opcode_DIVF:
    {
      int32 arg_sp = machine->sp-2;
      if(check_sp_bounds(machine, arg_sp))
      {
        switch(opcode)
        {
          case Opcode_ADD:
          case Opcode_SUB:
          case Opcode_MUL:
          case Opcode_DIV:
          case Opcode_MOD:
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
            else if(opcode == Opcode_MOD) {
              if(arg2 != 0)
                result = arg1 % arg2;
              else
                return ExecResult_DivByZero;
            }
            else if(opcode == Opcode_DIV)
            {
              if(arg2 != 0)
                result = arg1 / arg2;
              else
                return ExecResult_DivByZero;
            }
            else
              assert(0);

            *(int32*)&memory[arg_sp*VMWORD] = result;
          }
          break;

          case Opcode_ADDF:
          case Opcode_SUBF:
          case Opcode_MULF:
          case Opcode_DIVF:
          {
            float32 arg1 = *(float32*)&memory[arg_sp*VMWORD];
            float32 arg2 = *(float32*)&memory[(arg_sp+1)*VMWORD];

            float32 result = 0;
            if(opcode == Opcode_ADDF)
            {
              result = arg1 + arg2;
            }
            else if(opcode == Opcode_SUBF)
            {
              result = arg1 - arg2;
            }
            else if(opcode == Opcode_MULF)
            {
              result = arg1 * arg2;
            }
            else if(opcode == Opcode_DIVF)
            {
              if(arg2 != 0)
                result = arg1 / arg2;
              else
                return ExecResult_DivByZero;
            }
            else
              assert(0);

            *(float32*)&memory[arg_sp*VMWORD] = result;
          }
          break;

          default:
            assert(0);
        }

        machine->sp--;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_AND:
    case Opcode_OR:
    {
      int32 arg_sp = machine->sp-2;
      if(check_sp_bounds(machine, arg_sp))
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
          assert(0);

        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->sp--;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_NOT:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg = *(int32*)&memory[arg_sp*VMWORD];
        *(int32*)&memory[arg_sp*VMWORD] = (bool32)!arg;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_FLOAT_TO_INT:
    case Opcode_INT_TO_FLOAT:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        if(opcode == Opcode_FLOAT_TO_INT)
        {
          float32 arg = *(float32*)&memory[arg_sp*VMWORD];
          *(int32*)&memory[arg_sp*VMWORD] = (int32)arg;
        }
        else if(opcode == Opcode_INT_TO_FLOAT)
        {
          int32 arg = *(int32*)&memory[arg_sp*VMWORD];
          *(float32*)&memory[arg_sp*VMWORD] = (float32)arg;
        }

        machine->ip++;
      }
    }
    break;

    case Opcode_NEG:
    case Opcode_NEGF:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        if(opcode == Opcode_NEG)
        {
          int32 arg = *(int32*)&memory[arg_sp*VMWORD];
          *(int32*)&memory[arg_sp*VMWORD] = -arg;
        }
        else if(opcode == Opcode_NEGF)
        {
          float32 arg = *(float32*)&memory[arg_sp*VMWORD];
          *(float32*)&memory[arg_sp*VMWORD] = -arg;
        }
        else
          assert(0);

        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_INCR:
    case Opcode_DECR:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
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
    } break;

    case Opcode_LOAD:
    case Opcode_LOAD8:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
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
            assert(0);

          machine->ip++;
        }
        else
          return ExecResult_InvalidMemoryAccess;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_STORE:
    case Opcode_STORE8:
    {
      int32 loc_arg_sp = machine->sp-1;
      int32 val_arg_sp = machine->sp-2;
      if(check_sp_bounds(machine, val_arg_sp))
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
            assert(0);

          machine->sp--;
          machine->ip++;
        }
        else
          return ExecResult_InvalidMemoryAccess;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_DUP:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 value = *(int32*)&memory[arg_sp*VMWORD];
        if(check_sp_bounds(machine, machine->sp))
        {
          *(int32*)&memory[machine->sp*VMWORD] = value;
          machine->sp++;
          machine->ip++;
        }
        else
          return ExecResult_InvalidMemoryAccess;
      }
      else
        return ExecResult_InvalidMemoryAccess;
      } break;

    case Opcode_GOTO:
    {
      assert(instr->param_type == ParamType_Int32);
      machine->ip = instr->param.int_val;
    } break;

    case Opcode_CALL:
    {
      assert(instr->param_type == ParamType_Int32);
      int32 top_sp = machine->sp+3;
      if(check_sp_bounds(machine, top_sp))
      {
        *(int32*)&memory[machine->sp*VMWORD] = machine->ip+1;
        *(int32*)&memory[(machine->sp+1)*VMWORD] = machine->fp;
        *(int32*)&memory[(machine->sp+2)*VMWORD] = machine->sp;

        int32 jump_address = instr->param.int_val;
        machine->ip = jump_address;
        machine->sp = top_sp;
        machine->fp = top_sp;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_RETURN:
    {
      int32 arg_sp = machine->fp-3;
      if(check_sp_bounds(machine, arg_sp))
      {
        machine->ip = *(int32*)&memory[arg_sp*VMWORD];
        machine->fp = *(int32*)&memory[(arg_sp+1)*VMWORD];
        machine->sp = *(int32*)&memory[(arg_sp+2)*VMWORD];
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_ENTER:
    {
      int32 top_sp = machine->sp+1;
      if(check_sp_bounds(machine, top_sp))
      {
        *(int32*)&memory[machine->sp*VMWORD] = machine->fp;
        machine->fp = top_sp;
        machine->sp = top_sp;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_LEAVE:
    {
      int32 arg_sp = machine->fp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        machine->fp = *(int32*)&memory[arg_sp*VMWORD];
        machine->sp = arg_sp;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_JUMPZ:
    case Opcode_JUMPNZ:
    {
      assert(instr->param_type == ParamType_Int32);
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 jump_address = instr->param.int_val;
        int32 check = *(int32*)&memory[arg_sp*VMWORD];
        if((check && opcode == Opcode_JUMPNZ) ||
           (!check && opcode == Opcode_JUMPZ))
        {
          machine->ip = jump_address;
        }
        else
          machine->ip++;
        machine->sp--;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_CMPEQ:
    case Opcode_CMPNEQ:
    {
      int32 arg_sp = machine->sp-2;
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg1 = *(int32*)&memory[arg_sp*VMWORD];
        int32 arg2 = *(int32*)&memory[(arg_sp+1)*VMWORD];
        int32 result = (arg1 == arg2);
        if(opcode == Opcode_CMPNEQ)
          result = !result;
        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->sp--;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_CMPLSS:
    case Opcode_CMPGRT:
    {
      int32 arg_sp = machine->sp-2;
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg1 = *(int32*)&memory[arg_sp*VMWORD];
        int32 arg2 = *(int32*)&memory[(arg_sp+1)*VMWORD];
        int32 result = (arg1 < arg2);
        if(opcode == Opcode_CMPGRT)
          result = (arg1 > arg2);
        *(int32*)&memory[arg_sp*VMWORD] = result;
        machine->sp--;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_PRINT:
    {
      int32 arg_sp = machine->sp-1;
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg = *(int32*)&memory[arg_sp*VMWORD];
        printf("%d", arg);

        machine->sp = arg_sp;
        machine->ip++;
      }
      else
        return ExecResult_InvalidMemoryAccess;
    } break;

    case Opcode_PRINTNL:
    {
      putchar('\n');
      machine->ip++;
    } break;

    case Opcode_HALT:
        return ExecResult_EndOfProgram;

    case Opcode_LABEL:
    {
      assert(instr->param_type == ParamType_Int32);
      int32 jump_address = instr->param.int_val;
      machine->ip = jump_address;
    } break;

    case Opcode_NOOP:
    {
      machine->ip++;
    } break;

    default:
        return ExecResult_IllegalInstruction;
  }

  return ExecResult_OK;
}

local ExecResult
run_program(HocMachine* machine)
{
  BinCode* code = machine->code;

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
#if 1
    //memory dump
    for(int i = 0; i <= VMWORD*30; i += VMWORD)
      printf("%d ", *(int32*)&machine->memory[i]);
    printf("\n------------------------------------\n");
    for(int i = 0; i <= VMWORD*30; i += VMWORD)
      printf("%f ", *(float32*)&machine->memory[i]);
#endif
  }
  else {
    switch(exec_result)
    {
      case ExecResult_InvalidMemoryAccess:
        error("access to invalid memory location");
        break;
      case ExecResult_InvalidInstructionAddress:
        error("invalid instruction address");
        break;
      case ExecResult_IllegalInstruction:
        error("illegal instruction");
        break;
      case ExecResult_InvalidOperandSize:
        error("invalid operand size");
        break;
      case ExecResult_DivByZero:
        error("attemp to divide by zero");
        break;

      default:
        assert(0);
    }
  }

  return exec_result;
}

local bool32
load_bincode(char* exe_file_name, BinCode* code)
{
  uint8* exe_bytes = 0;
  int exe_size = 0;
  bool32 success = true;

  //TODO: Memory-map the file; we don't have to read the entire EXE image.
  file_read_bytes(arena, &exe_bytes, exe_file_name);

  IMAGE_DOS_HEADER* dos_header = (IMAGE_DOS_HEADER*)exe_bytes;
  if(cstr_match((char*)&dos_header->e_magic, "MZ\x90"))
  {
    IMAGE_NT_HEADERS* nt_header = (IMAGE_NT_HEADERS*)(exe_bytes + dos_header->e_lfanew);
    if(cstr_match((char*)&nt_header->Signature, "PE"))
    {
      int nr_sections = nt_header->FileHeader.NumberOfSections;
      IMAGE_SECTION_HEADER* section_header = (IMAGE_SECTION_HEADER*)((uint8*)nt_header + sizeof(IMAGE_NT_HEADERS));
      exe_size = nt_header->OptionalHeader.SizeOfHeaders;
      for(int i = 0; i < nr_sections; i++)
      {
        exe_size += section_header->SizeOfRawData;
        section_header++;
      }
    }
  }

  if(exe_size > 0)
  {
    BinCode* image_code = (BinCode*)(exe_bytes + exe_size);

    if(cstr_match(image_code->sig, BINCODE_SIGNATURE))
    {
      // Fix the pointers
      *code = *image_code;
      code->code = (uint8*)image_code;
      code->instr_array = (Instruction*)(code->code + sizeof(BinCode));
    }
    else
      success = error("bincode signature mismatch");
  }
  else
    success = error("could not read the EXE file size of `%s`", exe_file_name);
  return success;
}

int
main(int argc, char* argv[])
{
  int ret = -1;

  arena = arena_new(2*MEGABYTE);

  HocMachine machine = {0};
  BinCode code = {0};
  assert(argv[0] && argv[0] != '\0');

  if(load_bincode(argv[0], &code))
  {
    machine.memory = mem_push_count(arena, uint8, VM_MEMORY_SIZE);
    machine.memory_size = VM_MEMORY_SIZE;
    machine.code = &code;

    machine.sp++; // 0-th cell reserved
    machine.hp = machine.memory_size/VMWORD;
    ret = (int)run_program(&machine);
  }
  return ret;
}

