#include "hocc.h"

#define VM_MEMORY_SIZE 1024

#define memory_at(LOC, TYPE, OFFSET)\
  *((TYPE*)&machine->memory[(LOC)] + (OFFSET))

#define memory_loc(LOC, TYPE, OFFSET)\
  ((LOC) + (int32)sizeof(TYPE)*(OFFSET))

typedef struct
{
  Instruction* instructions;
  int instr_count;

  uint8* data;
  int data_size;

  int32 fp;
  int32 sp;
  int32 hp;
  int32 ip;
  int memory_size;
  uint8* memory;
}
HocMachine;

typedef enum
{
  Result_EndOfProgram = 0,
  Result_OK,
  Result_InvalidMemoryAccess,
  Result_InvalidInstructionAddress,
  Result_InvalidInstructionFormat,
  Result_DivByZero,

  Result__Count
}
ExecResult;

local MemoryArena* arena;

local bool32
check_sp_bounds(HocMachine* machine, int sp)
{
  return sp > 0 && sp < machine->hp;
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
check_instr_bounds(HocMachine* machine, int address)
{
  return address >= 0 && address < machine->instr_count;
}

local void
clear_memory(HocMachine* machine, int base, int count)
{
  for(int i = 0; i < count; i++)
    machine->memory[base + i] = 0xcd;
}

local int32
offset_register(int32 reg, int32 offset)
{
  return reg + offset*sizeof(int32);
}

local ExecResult
execute_instr(HocMachine* machine, Instruction* instr)
{
  Opcode opcode = instr->opcode;

  switch(opcode)
  {
    case Opcode_ALLOC:
    {
      if(instr->param_type == ParamType_Int32)
      {
        int32 top_sp = machine->sp + instr->param.int_val;
        if(check_sp_bounds(machine, top_sp))
        {
          clear_memory(machine, machine->sp, instr->param.int_val);

          machine->sp = top_sp;
          machine->ip++;
        } 
        else
          return Result_InvalidMemoryAccess;
      }
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_NEW:
    {
      if(instr->param_type == ParamType_Int32)
      {
        int32 top_hp = machine->hp - instr->param.int_val;
        int32 top_sp = machine->sp + 1*sizeof(int32);
        if((top_hp < machine->hp) && check_hp_bounds(machine, top_hp) && check_sp_bounds(machine, top_sp))
        {
          clear_memory(machine, top_hp, instr->param.int_val);

          memory_at(machine->sp, int32, 0) = top_hp;

          machine->hp = top_hp;
          machine->sp++;
          machine->ip++;
        }
        else
          return Result_InvalidMemoryAccess;
      }
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_PUSH_I8:
    {
      int32 top_sp = memory_loc(machine->sp, int8, 1);
      if(check_sp_bounds(machine, top_sp))
      {
        if(instr->param_type == ParamType_Int32)
        {
          if(instr->param.int_val <= 0xff)
            memory_at(machine->sp, int8, 0) = (int8)instr->param.int_val;
          else
            return Result_InvalidInstructionFormat;
        }
        else
          return Result_InvalidInstructionFormat;

        machine->sp = top_sp;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_PUSH_I32:
    {
      int32 top_sp = memory_loc(machine->sp, int32, 1);
      if(check_sp_bounds(machine, top_sp))
      {
        if(instr->param_type == ParamType_Int32)
          memory_at(machine->sp, int32, 0) = instr->param.int_val;
        else
          return Result_InvalidInstructionFormat;

        machine->sp = top_sp;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_PUSH_F32:
    {
      int32 top_sp = memory_loc(machine->sp, float32, 1);
      if(check_sp_bounds(machine, top_sp))
      {
        if(instr->param_type == ParamType_Float32)
          memory_at(machine->sp, float32, 0) = instr->param.float_val;
        else
          return Result_InvalidInstructionFormat;

        machine->sp = top_sp;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_PUSH_R:
    {
      int32 top_sp = memory_loc(machine->sp, int32, 1);
      if(check_sp_bounds(machine, top_sp))
      {
        if(instr->param_type == ParamType_Reg)
        {
          RegName regname = instr->param.reg;
          if(regname == RegName_SP)
            memory_at(machine->sp, int32, 0) = machine->sp;
          else if(regname == RegName_IP)
            memory_at(machine->sp, int32, 0) = machine->ip;
          else if(regname == RegName_FP)
            memory_at(machine->sp, int32, 0) = machine->fp;
          else
            return Result_InvalidInstructionFormat;
        }
        else
          return Result_InvalidInstructionFormat;

        machine->sp = top_sp;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_POP_R:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        if(instr->param_type == ParamType_Reg)
        {
          RegName regname = instr->param.reg;
          if(regname == RegName_SP)
          {
            machine->sp = memory_at(arg_sp, int32, 0);
            machine->ip++;
          }
          else if(regname == RegName_IP)
          {
            int32 ip = memory_at(arg_sp, int32, 0);
            machine->sp = arg_sp;
            machine->ip = ip;
          }
          else if(regname == RegName_FP)
          {
            machine->fp = memory_at(arg_sp, int32, 0);
            machine->sp = arg_sp;
            machine->ip++;
          }
          else
            return Result_InvalidInstructionFormat;
        }
        else
          return Result_InvalidInstructionFormat;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_POP:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        if(instr->param_type == ParamType_Int32)
        {
          machine->sp = memory_loc(machine->sp, int8, -instr->param.int_val);
          machine->ip++;
        }
        else
          return Result_InvalidInstructionFormat;
      }
      else
        return Result_InvalidMemoryAccess;
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
      int32 arg_sp = memory_loc(machine->sp, int32, -2);
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
            int32 arg1 = memory_at(arg_sp, int32, 0);
            int32 arg2 = memory_at(arg_sp, int32, 1);

            int32 result = 0;
            if(opcode == Opcode_ADD)
              result = arg1 + arg2;
            else if(opcode == Opcode_SUB)
              result = arg1 - arg2;
            else if(opcode == Opcode_MUL)
              result = arg1 * arg2;
            else if(opcode == Opcode_MOD)
            {
              if(arg2 != 0)
                result = arg1 % arg2;
              else
                return Result_DivByZero;
            }
            else if(opcode == Opcode_DIV)
            {
              if(arg2 != 0)
                result = arg1 / arg2;
              else
                return Result_DivByZero;
            }
            else
              assert(0);

            memory_at(arg_sp, int32, 0) = result;
          }
          break;

          case Opcode_ADDF:
          case Opcode_SUBF:
          case Opcode_MULF:
          case Opcode_DIVF:
          {
            float32 arg1 = memory_at(arg_sp, float32, 0);
            float32 arg2 = memory_at(arg_sp, float32, 1);

            float32 result = 0;
            if(opcode == Opcode_ADDF)
              result = arg1 + arg2;
            else if(opcode == Opcode_SUBF)
              result = arg1 - arg2;
            else if(opcode == Opcode_MULF)
              result = arg1 * arg2;
            else if(opcode == Opcode_DIVF)
            {
              if(arg2 != 0)
                result = arg1 / arg2;
              else
                return Result_DivByZero;
            }
            else
              assert(0);

            memory_at(arg_sp, float32, 0) = result;
          }
          break;

          default:
            assert(0);
        }

        machine->sp = memory_loc(machine->sp, int32, -1);
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_AND:
    case Opcode_OR:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -2);
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg1 = memory_at(arg_sp, int32, 0);
        int32 arg2 = memory_at(arg_sp, int32, 1);

        bool32 result = 0;
        if(opcode == Opcode_AND)
          result = arg1 && arg2;
        else if(opcode == Opcode_OR)
          result = arg1 || arg2;
        else
          assert(0);

        memory_at(arg_sp, int32, 0) = result;
        machine->sp = memory_loc(machine->sp, int32, -1);
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_NOT:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg = memory_at(arg_sp, int32, 0);
        memory_at(arg_sp, int32, 0) = (bool32)!arg;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_FLOAT_TO_INT:
    case Opcode_INT_TO_FLOAT:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        if(opcode == Opcode_FLOAT_TO_INT)
        {
          float32 arg = memory_at(arg_sp, float32, 0);
          memory_at(arg_sp, int32, 0) = (int32)arg;
        }
        else if(opcode == Opcode_INT_TO_FLOAT)
        {
          int32 arg = memory_at(arg_sp, int32, 0);
          memory_at(arg_sp, float32, 0) = (float32)arg;
        }

        machine->ip++;
      }
    }
    break;

    case Opcode_NEG:
    case Opcode_NEGF:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        if(opcode == Opcode_NEG)
        {
          int32 arg = memory_at(arg_sp, int32, 0);
          memory_at(arg_sp, int32, 0) = -arg;
        }
        else if(opcode == Opcode_NEGF)
        {
          float32 arg = memory_at(arg_sp, float32, 0);
          memory_at(arg_sp, float32, 0) = -arg;
        }
        else
          assert(0);

        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_INCR:
    case Opcode_DECR:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg = memory_at(arg_sp, int32, 0);
        int32 result = arg;
        if(opcode == Opcode_INCR)
          result++;
        else
          result--;
        memory_at(arg_sp, int32, 0) = result;
        machine->ip++;
      }
    } break;

    case Opcode_LOAD:
    {
      if(instr->param_type == ParamType_Int32)
      {
        if(instr->param.int_val == 4)
        {
          int32 arg_sp = memory_loc(machine->sp, int32, -1);
          if(check_sp_bounds(machine, arg_sp))
          {
            int32 location = memory_at(arg_sp, int32, 0);
            if(check_memory_bounds(machine, location))
            {
              int32 value = memory_at(location, int32, 0);
              memory_at(arg_sp, int32, 0) = value;

              machine->ip++;
            }
            else
              return Result_InvalidMemoryAccess;
          }
          else
            return Result_InvalidMemoryAccess;
        }
        else if(instr->param.int_val == 1)
          fail("not implemented");
        else
          return Result_InvalidInstructionFormat;
      }
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_STORE:
    {
      if(instr->param_type == ParamType_Int32)
      {
        if(instr->param.int_val == 4)
        {
          if(check_sp_bounds(machine, memory_loc(machine->sp, int32, -2)))
          {
            int32 location = memory_at(machine->sp, int32, -1);
            if(check_memory_bounds(machine, location))
            {
              int32 value = memory_at(machine->sp, int32, -2);
              memory_at(location, int32, 0) = value;

              machine->sp = memory_loc(machine->sp, int32, -1);
              machine->ip++;
            }
            else
              return Result_InvalidMemoryAccess;
          }
          else
            return Result_InvalidMemoryAccess;
        }
        else if(instr->param.int_val == 1)
        {
          if(check_sp_bounds(machine, memory_loc(machine->sp, int8, -5)))
          {
            int32 location = memory_at(machine->sp, int32, -1);
            if(check_memory_bounds(machine, location))
            {
              int8 value = memory_at(machine->sp, int8, -5);
              memory_at(location, int8, 0) = value;

              machine->sp = memory_loc(machine->sp, int32, -1);
              machine->ip++;
            }
            else
              return Result_InvalidMemoryAccess;
          }
          else
            return Result_InvalidMemoryAccess;
        }
        else
          return Result_InvalidInstructionFormat;
      }
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_DUP:
    {
      if(check_sp_bounds(machine, memory_loc(machine->sp, int32, -1)))
      {
        int32 value = memory_at(machine->sp, int32, -1);
        if(check_sp_bounds(machine, machine->sp))
        {
          memory_at(machine->sp, int32, 0) = value;
          machine->sp = memory_loc(machine->sp, int32, 1);
          machine->ip++;
        }
        else
          return Result_InvalidMemoryAccess;
      }
      else
        return Result_InvalidMemoryAccess;
      } break;

    case Opcode_GOTO:
    {
      if(instr->param_type == ParamType_Int32)
        machine->ip = instr->param.int_val;
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_CALL:
    {
      if(instr->param_type == ParamType_Int32)
      {
        int32 top_sp = memory_loc(machine->sp, int32, 3);
        if(check_sp_bounds(machine, top_sp))
        {
          memory_at(machine->sp, int32, 0) = machine->ip + 1;
          memory_at(machine->sp, int32, 1) = machine->sp;
          memory_at(machine->sp, int32, 2) = machine->fp;

          int32 jump_address = instr->param.int_val;
          machine->ip = jump_address;
          machine->sp = top_sp;
          machine->fp = top_sp;
        }
        else
          return Result_InvalidMemoryAccess;
      }
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_RETURN:
    {
      int32 arg_sp = memory_loc(machine->fp, int32, -3);
      if(check_sp_bounds(machine, arg_sp))
      {
        machine->ip = memory_at(arg_sp, int32, 0);
        machine->sp = memory_at(arg_sp, int32, 1);
        machine->fp = memory_at(arg_sp, int32, 2);
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_ENTER:
    {
      int32 top_sp = memory_loc(machine->sp, int32, 1);
      if(check_sp_bounds(machine, top_sp))
      {
        memory_at(machine->sp, int32, 0) = machine->fp;
        machine->fp = top_sp;
        machine->sp = top_sp;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_LEAVE:
    {
      int32 arg_sp = memory_loc(machine->fp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        machine->fp = memory_at(arg_sp, int32, 0);
        machine->sp = arg_sp;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_JUMPZ:
    case Opcode_JUMPNZ:
    {
      if(instr->param_type == ParamType_Int32)
      {
        int32 arg_sp = memory_loc(machine->sp, int32, -1);
        if(check_sp_bounds(machine, arg_sp))
        {
          int32 jump_address = instr->param.int_val;
          int32 check = memory_at(arg_sp, int32, 0);
          if((check && opcode == Opcode_JUMPNZ) || (!check && opcode == Opcode_JUMPZ))
            machine->ip = jump_address;
          else
            machine->ip++;
          machine->sp = arg_sp;
        }
        else
          return Result_InvalidMemoryAccess;
      }
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_CMPEQ:
    case Opcode_CMPNEQ:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -2);
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg1 = memory_at(arg_sp, int32, 0);
        int32 arg2 = memory_at(arg_sp, int32, 1);
        int32 result = (arg1 == arg2);
        if(opcode == Opcode_CMPNEQ)
          result = !result;
        memory_at(arg_sp, int32, 0) = result;
        machine->sp = memory_loc(machine->sp, int32, -1);
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_CMPLSS:
    case Opcode_CMPGRT:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -2);
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg1 = memory_at(arg_sp, int32, 0);
        int32 arg2 = memory_at(arg_sp, int32, 1);
        int32 result = (arg1 < arg2);
        if(opcode == Opcode_CMPGRT)
          result = (arg1 > arg2);
        memory_at(arg_sp, int32, 0) = result;
        machine->sp = memory_loc(machine->sp, int32, -1);
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_PRINT:
    {
      int32 arg_sp = memory_loc(machine->sp, int32, -1);
      if(check_sp_bounds(machine, arg_sp))
      {
        int32 arg = memory_at(arg_sp, int32, 0);
        printf("%d", arg);

        machine->sp = arg_sp;
        machine->ip++;
      }
      else
        return Result_InvalidMemoryAccess;
    } break;

    case Opcode_HALT:
        return Result_EndOfProgram;

    case Opcode_LABEL:
    {
      if(instr->param_type == ParamType_Int32)
      {
        int32 jump_address = instr->param.int_val;
        machine->ip = jump_address;
      }
      else
        return Result_InvalidInstructionFormat;
    } break;

    case Opcode_NOOP:
    {
      machine->ip++;
    } break;

    default:
        return Result_InvalidInstructionFormat;
  }

  return Result_OK;
}

local ExecResult
run_program(HocMachine* machine)
{
  Instruction* instr;
  ExecResult exec_result = Result_OK;
  do
  {
    int ip = machine->ip;
    if(check_instr_bounds(machine, ip))
    {
      instr = &machine->instructions[ip];
      exec_result = execute_instr(machine, instr);
    }
    else
      exec_result = Result_InvalidInstructionAddress;
  }
  while(exec_result == Result_OK);

  if(exec_result == Result_EndOfProgram)
  {
#if 0
    //memory dump
    for(int i = 0; i <= VMWORD*30; i += VMWORD)
      printf("%d ", *(uint32*)&machine->memory[i]);
    printf("\n------------------------------------\n");
    for(int i = 0; i <= VMWORD*30; i += VMWORD)
      printf("%f ", *(float32*)&machine->memory[i]);
#endif
  }
  else {
    switch(exec_result)
    {
      case Result_InvalidMemoryAccess:
        error("access to invalid memory location");
        break;
      case Result_InvalidInstructionAddress:
        error("invalid instruction address");
        break;
      case Result_InvalidInstructionFormat:
        error("invalid instruction format");
        break;
      case Result_DivByZero:
        error("attemp to divide by zero");
        break;

      default:
        assert(0);
    }
  }

  return exec_result;
}

local bool32
load_bin_image(char* exe_file_name, HocMachine* machine)
{
  uint8* exe_bytes = 0;
  int exe_size = 0;
  bool32 success = true;

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
    uint8* hoc_base = exe_bytes + exe_size;
    BinCode* bin_image = (BinCode*)(hoc_base);

    if(cstr_match(bin_image->sig, BINCODE_SIGNATURE))
    {
      machine->instructions = (Instruction*)(hoc_base + bin_image->code_offset);
      machine->instr_count = bin_image->code_size / sizeof(Instruction);

      machine->data = (uint8*)(hoc_base + bin_image->data_offset);
      machine->data_size = bin_image->data_size / sizeof(uint8);

      for(int i = 0; i < machine->data_size; i++)
        machine->memory[i] = machine->data[i];
    }
    else
      success = error("bincode signature mismatch");
  }
  else
    success = error("could not read the EXE size of `%s`", exe_file_name);
  return success;
}

int
main(int argc, char* argv[])
{
  int ret = -1;
  assert(argv[0] && argv[0] != '\0');

  arena = arena_new(2*MEGABYTE);

  HocMachine machine = {0};
  machine.memory = (uint8*)mem_push_count(arena, uint8, VM_MEMORY_SIZE);
  machine.memory_size = VM_MEMORY_SIZE;

  if(load_bin_image(argv[0], &machine))
  {
    ((uint8*)machine.memory)[0] = 0; // null ptr cell

    machine.sp = machine.data_size; // the null ptr cell is counted in
    machine.fp = 0;
    machine.hp = machine.memory_size;

    ret = (int)run_program(&machine);
  }
  return ret;
}

