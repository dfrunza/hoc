#include "lib.c"
#include "hasm.h"

typedef struct
{
  char* string;
  int instr_address;
}
Label;

typedef struct
{
  char* text;

  int max_lines;
  int line_count;
  InstructionLine lines[1024];

  int max_labels;
  int labesl_count;
  Label labels[512]; //FIXME: Unchecked bounds
}
SourceProgram;

int break_instr_into_components(char* str, char* components[], int max_component_count)
{/*>>>*/
  int component_count = 0;

  char* component = str;
  char* char_ptr = str;

  do
  {
    while(*char_ptr != '\0' && *char_ptr != ' ' && *char_ptr != ';')
      char_ptr++;

    if(component_count < max_component_count)
      components[component_count++] = component;
    else
      return max_component_count+1;

    if(*char_ptr != '\0')
    {
      if(*char_ptr == ';')
      {
        while(*char_ptr != '\0')
          *char_ptr++ = '\0';
      }
      else
      {
        while(*char_ptr == ' ')
          *char_ptr++ = '\0';
        component = char_ptr;
      }
    }
  } while(*char_ptr != '\0' && *char_ptr != ';');

  return component_count;
}/*<<<*/

int find_instr_address_at_line(SourceProgram* source, int lineNr)
{
  int result = -1;
  InstructionLine* line = source->lines;
  for(int line_index = 0; line_index < source->line_count; line_index++)
  {
    if(line->source_line_nr == lineNr)
    {
      result = line_index;
      break;
    }
    line++;
  }
  return result;
}

Label* find_label(SourceProgram* source, char* label_string)
{
  Label* result = 0;
  for(int i = 0; i < source->labesl_count; i++)
  {
    Label* label = &source->labels[i];
    if(str_equals(label->string, label_string))
    {
      result = label;
      break;
    }
  }
  return result;
}

bool32 is_valid_label(char *label)
{
  char start_char = *label;
  return ('A' <= start_char && start_char <= 'Z') || ('a' <= start_char && start_char <= 'z') || start_char == '.';
}

void process_source_lines(SourceProgram* source)
{/*>>>*/
  source->line_count = 0;
  char* char_ptr = &source->text[0];
  if(*char_ptr != '\0')
  {
    int source_line_nr = 1;

    if(*char_ptr != '\n' && *char_ptr != ';')
    {
      InstructionLine instr_line = {0};
      instr_line.source_line_nr = source_line_nr;
      instr_line.string = char_ptr;
      source->lines[source->line_count++] = instr_line;

      char_ptr++;
      while(*char_ptr != '\0')
      {
        if(*char_ptr == '\n')
        {
          source_line_nr++;

          char* next_char_ptr = char_ptr+1;
          if(*next_char_ptr != '\0' && *next_char_ptr != '\n' && *next_char_ptr != ';')
          {
            memset(&instr_line, 0, sizeof(instr_line));
            instr_line.source_line_nr = source_line_nr;
            instr_line.string = next_char_ptr;
            source->lines[source->line_count++] = instr_line;
          }
          *char_ptr = '\0';
        }
        char_ptr++;
      }
    }
  }
}/*<<<*/

bool32 build_ir_code(MemoryArena* arena, SourceProgram* source, HasmCode** out_code)
{/*>>>*/
  HasmCode* code = push_element(arena, HasmCode, 1);
  copy_cstr(code->groove, "IRC");
  code->code_start = (uint8*)code;
  code->instr_count = source->line_count;
  code->instr_array = push_element(arena, Instruction, code->instr_count);
  code->code_size = (int)((uint8*)arena->free - code->code_start);
  *out_code = code;

  for(int instr_address = 0; instr_address < code->instr_count; instr_address++)
  {
    Instruction instr = {0};
    InstructionLine* instr_line = &source->lines[instr_address];
    instr.source_line_nr = instr_line->source_line_nr;

    char* components[2] = {0};
    int component_count = break_instr_into_components(instr_line->string, components, sizeof_array(components));
    if(component_count >= 1 && component_count <= sizeof_array(components))
    {
      char* mnemonic = components[0];

      if(str_equals(mnemonic, "pop"))
        instr.opcode = Opcode_POP;
      else if(str_equals(mnemonic, "push"))
        instr.opcode = Opcode_PUSH;
      else if(str_equals(mnemonic, "store"))
        instr.opcode = Opcode_STORE;
      else if(str_equals(mnemonic, "store8"))
        instr.opcode = Opcode_STORE8;
      else if(str_equals(mnemonic, "load"))
        instr.opcode = Opcode_LOAD;
      else if(str_equals(mnemonic, "load8"))
        instr.opcode = Opcode_LOAD8;
      else if(str_equals(mnemonic, "add"))
        instr.opcode = Opcode_ADD;
      else if(str_equals(mnemonic, "sub"))
        instr.opcode = Opcode_SUB;
      else if(str_equals(mnemonic, "mul"))
        instr.opcode = Opcode_MUL;
      else if(str_equals(mnemonic, "div"))
        instr.opcode = Opcode_DIV;
      else if(str_equals(mnemonic, "mod"))
        instr.opcode = Opcode_MOD;
      else if(str_equals(mnemonic, "incr"))
        instr.opcode = Opcode_INCR;
      else if(str_equals(mnemonic, "decr"))
        instr.opcode = Opcode_DECR;
      else if(str_equals(mnemonic, "halt"))
        instr.opcode = Opcode_HALT;
      else if(str_equals(mnemonic, "print"))
        instr.opcode = Opcode_PRINT;
      else if(str_equals(mnemonic, "dup"))
        instr.opcode = Opcode_DUP;
      else if(str_equals(mnemonic, "goto"))
        instr.opcode = Opcode_GOTO;
      else if(str_equals(mnemonic, "jumpnz"))
        instr.opcode = Opcode_JUMPNZ;
      else if(str_equals(mnemonic, "jumpz"))
        instr.opcode = Opcode_JUMPZ;
      else if(str_equals(mnemonic, "label"))
        instr.opcode = Opcode_LABEL;
      else if(str_equals(mnemonic, "noop"))
        instr.opcode = Opcode_NOOP;
      else if(str_equals(mnemonic, "call"))
        instr.opcode = Opcode_CALL;
      else if(str_equals(mnemonic, "return"))
        instr.opcode = Opcode_RETURN;
      else if(str_equals(mnemonic, "enter"))
        instr.opcode = Opcode_ENTER;
      else if(str_equals(mnemonic, "leave"))
        instr.opcode = Opcode_LEAVE;
      else if(str_equals(mnemonic, "alloc"))
        instr.opcode = Opcode_ALLOC;
      else {
        error("Invalid instruction: %s", mnemonic);
        return false;
      }

      if(component_count == 2)
      {
        switch(instr.opcode)
        {
          case Opcode_GOTO:
          case Opcode_JUMPNZ:
          case Opcode_JUMPZ:
          case Opcode_CALL:
            {
              if(is_valid_label(components[1]))
              {
                instr.param_type = ParamType_String;
                instr.param.str = components[1];
              }
              else {
                error("Label must begin with a letter : %s", components[1]);
                return false;
              }
            } break;

          case Opcode_POP:
          case Opcode_PUSH:
            {
              if(str_to_int(components[1], &instr.param.int_num))
                instr.param_type = ParamType_Int32;
              else {
                instr.param_type = ParamType_Reg;
                char* reg = components[1];

                if(str_match(reg, "sp"))
                  instr.param.reg = RegName_SP;
                else if(str_match(reg, "fp"))
                  instr.param.reg = RegName_FP;
                else if(str_match(reg, "ip"))
                  instr.param.reg = RegName_IP;
                else {
                  error("Invalid register '%s'", components[1]);
                  return false;
                }
              }
            } break;

          case Opcode_ALLOC:
            {
              if(str_to_int(components[1], &instr.param.int_num))
                instr.param_type = ParamType_Int32;
              else {
                error("Invalid parameter '%s'", components[1]);
                return false;
              }
            } break;

          case Opcode_LABEL:
            {
              if(is_valid_label(components[1]))
              {
                instr.param_type = ParamType_String;
                instr.param.str = components[1];

                Label label = {0};
                label.string = components[1];
                label.instr_address = instr_address;

                if(!find_label(source, label.string))
                  source->labels[source->labesl_count++] = label;
                else {
                  error("Duplicate label declaration '%s'", label.string);
                  return false;
                }
              } else {
                error("Label '%s' does not begin with a letter", components[1]);
                return false;
              }
            } break;

          default:
            error("Incorrect number of parameters to instruction '%s'", mnemonic);
            return false;
        }
      }

      code->instr_array[instr_address] = instr;
    }
    else
    {
      error("Illegal number of instruction components");
      return false;
    }
  }

  for(int labelIndex = 0; labelIndex < source->labesl_count; labelIndex++)
  {
    Label* label = &source->labels[labelIndex];
    int target_instr_address = label->instr_address+1;
    for(; target_instr_address < code->instr_count; target_instr_address++)
    {
      Instruction* instr = &code->instr_array[target_instr_address];
      if(instr->opcode != Opcode_LABEL)
        break;
    }
    if(target_instr_address < code->instr_count)
    {
      Instruction* label_instr = &code->instr_array[label->instr_address];
      label_instr->param_type = ParamType_Int32;
      label_instr->param.int_num = target_instr_address;
      label->instr_address = target_instr_address;
    }
    else {
      error("Could not find a non-label instruction following the label %s", label->string);
      return false;
    }
  }

  for(int instr_address = 0; instr_address < code->instr_count; instr_address++)
  {
    Instruction* instr = &code->instr_array[instr_address];
    if(instr->opcode == Opcode_GOTO ||
       instr->opcode == Opcode_JUMPNZ ||
       instr->opcode == Opcode_JUMPZ ||
       instr->opcode == Opcode_CALL)
    {
      assert(instr->param_type == ParamType_String);
      Label* label = find_label(source, instr->param.str);
      if(label)
      {
        instr->param_type = ParamType_Int32;
        instr->param.int_num = label->instr_address;
      }
      else {
        error("Label '%s' not found", instr->param.str);
        return false;
      }
    }
  }

  return true;
}/*<<<*/

bool32 translate_ir_to_code(MemoryArena* arena, char* text, HasmCode** code)
{
  bool32 success = true;

  SourceProgram source = {0};
  source.text = text;
  process_source_lines(&source);

  success = build_ir_code(arena, &source, code);
  return success;
}
