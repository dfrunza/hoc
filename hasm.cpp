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
  InstructionLine lines[8192];

  int max_labels;
  int labesl_count;
  Label labels[1024]; //FIXME: Unchecked bounds
}
SourceProgram;

int
instr_to_components(char* str, char* components[], int max_component_count)
{
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
  }
  while(*char_ptr != '\0' && *char_ptr != ';');

  return component_count;
}

#if 0
int
find_instr_address_at_line(SourceProgram* source, int lineNr)
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
#endif

Label*
find_label(SourceProgram* source, char* label_string)
{
  Label* result = 0;
  for(int i = 0; i < source->labesl_count; i++)
  {
    Label* label = &source->labels[i];
    if(cstr_match(label->string, label_string))
    {
      result = label;
      break;
    }
  }
  return result;
}

boole
is_valid_label(char *label)
{
  char start_char = *label;
  return ('A' <= start_char && start_char <= 'Z') || ('a' <= start_char && start_char <= 'z') || start_char == '.';
}

void
process_source_lines(SourceProgram* source)
{
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
}

boole
build_instructions(SourceProgram* source, VmProgram* vm_program)
{
  vm_program->instr_count = source->line_count;
  vm_program->instructions = mem_push_count_nz(arena, Instruction, vm_program->instr_count);

  for(int instr_address = 0; instr_address < vm_program->instr_count; instr_address++)
  {
    Instruction instr = {0};
    InstructionLine* instr_line = &source->lines[instr_address];
    instr.source_line_nr = instr_line->source_line_nr;

    char* components[2] = {0};
    int component_count = instr_to_components(instr_line->string, components, sizeof_array(components));
    if(component_count >= 1 && component_count <= sizeof_array(components))
    {
      char* mnemonic = components[0];

      if(cstr_match(mnemonic, "pop_reg"))
        instr.opcode = Opcode_POP_REG;
      else if(cstr_match(mnemonic, "push_char"))
        instr.opcode = Opcode_PUSH_CHAR;
      else if(cstr_match(mnemonic, "push_int"))
        instr.opcode = Opcode_PUSH_INT;
      else if(cstr_match(mnemonic, "push_float"))
        instr.opcode = Opcode_PUSH_FLOAT;
      else if(cstr_match(mnemonic, "push_reg"))
        instr.opcode = Opcode_PUSH_REG;
      else if(cstr_match(mnemonic, "store"))
        instr.opcode = Opcode_STORE;
      else if(cstr_match(mnemonic, "load"))
        instr.opcode = Opcode_LOAD;
      else if(cstr_match(mnemonic, "grow"))
        instr.opcode = Opcode_GROW;
      else if(cstr_match(mnemonic, "new"))
        instr.opcode = Opcode_NEW;

      else if(cstr_match(mnemonic, "add_int"))
        instr.opcode = Opcode_ADD_INT;
      else if(cstr_match(mnemonic, "sub_int"))
        instr.opcode = Opcode_SUB_INT;
      else if(cstr_match(mnemonic, "mul_int"))
        instr.opcode = Opcode_MUL_INT;
      else if(cstr_match(mnemonic, "div_int"))
        instr.opcode = Opcode_DIV_INT;
      else if(cstr_match(mnemonic, "mod_int"))
        instr.opcode = Opcode_MOD_INT;
      else if(cstr_match(mnemonic, "add_float"))
        instr.opcode = Opcode_ADD_FLOAT;
      else if(cstr_match(mnemonic, "sub_float"))
        instr.opcode = Opcode_SUB_FLOAT;
      else if(cstr_match(mnemonic, "mul_float"))
        instr.opcode = Opcode_MUL_FLOAT;
      else if(cstr_match(mnemonic, "div_float"))
        instr.opcode = Opcode_DIV_FLOAT;
      else if(cstr_match(mnemonic, "neg_float"))
        instr.opcode = Opcode_NEG_FLOAT;
      else if(cstr_match(mnemonic, "incr_int"))
        instr.opcode = Opcode_INCR_INT;
      else if(cstr_match(mnemonic, "decr_int"))
        instr.opcode = Opcode_DECR_INT;

      else if(cstr_match(mnemonic, "halt"))
        instr.opcode = Opcode_HALT;
      else if(cstr_match(mnemonic, "putc"))
        instr.opcode = Opcode_PUTC;
      else if(cstr_match(mnemonic, "dup"))
        instr.opcode = Opcode_DUP;
      else if(cstr_match(mnemonic, "goto"))
        instr.opcode = Opcode_GOTO;
      else if(cstr_match(mnemonic, "jumpnz"))
        instr.opcode = Opcode_JUMPNZ;
      else if(cstr_match(mnemonic, "jumpz"))
        instr.opcode = Opcode_JUMPZ;

      else if(cstr_match(mnemonic, "cmpeq_char"))
        instr.opcode = Opcode_CMPEQ_CHAR;
      else if(cstr_match(mnemonic, "cmpneq_char"))
        instr.opcode = Opcode_CMPNEQ_CHAR;
      else if(cstr_match(mnemonic, "cmplss_char"))
        instr.opcode = Opcode_CMPLSS_CHAR;
      else if(cstr_match(mnemonic, "cmpgrt_char"))
        instr.opcode = Opcode_CMPGRT_CHAR;

      else if(cstr_match(mnemonic, "cmpeq_int"))
        instr.opcode = Opcode_CMPEQ_INT;
      else if(cstr_match(mnemonic, "cmpneq_int"))
        instr.opcode = Opcode_CMPNEQ_INT;
      else if(cstr_match(mnemonic, "cmplss_int"))
        instr.opcode = Opcode_CMPLSS_INT;
      else if(cstr_match(mnemonic, "cmpgrt_int"))
        instr.opcode = Opcode_CMPGRT_FLOAT;

      else if(cstr_match(mnemonic, "cmpeq_float"))
        instr.opcode = Opcode_CMPEQ_FLOAT;
      else if(cstr_match(mnemonic, "cmpneq_float"))
        instr.opcode = Opcode_CMPNEQ_FLOAT;
      else if(cstr_match(mnemonic, "cmplss_float"))
        instr.opcode = Opcode_CMPLSS_FLOAT;
      else if(cstr_match(mnemonic, "cmpgrt_float"))
        instr.opcode = Opcode_CMPGRT_FLOAT;

      else if(cstr_match(mnemonic, "and"))
        instr.opcode = Opcode_AND;
      else if(cstr_match(mnemonic, "or"))
        instr.opcode = Opcode_OR;
      else if(cstr_match(mnemonic, "not"))
        instr.opcode = Opcode_NOT;
      else if(cstr_match(mnemonic, "neg_int"))
        instr.opcode = Opcode_NEG_INT;
      else if(cstr_match(mnemonic, "neg_float"))
        instr.opcode = Opcode_NEG_FLOAT;
      else if(cstr_match(mnemonic, "label"))
        instr.opcode = Opcode_LABEL;
      else if(cstr_match(mnemonic, "noop"))
        instr.opcode = Opcode_NOOP;
      else if(cstr_match(mnemonic, "call"))
        instr.opcode = Opcode_CALL;
      else if(cstr_match(mnemonic, "return"))
        instr.opcode = Opcode_RETURN;
      else if(cstr_match(mnemonic, "enter"))
        instr.opcode = Opcode_ENTER;
      else if(cstr_match(mnemonic, "leave"))
        instr.opcode = Opcode_LEAVE;

      else if(cstr_match(mnemonic, "float_to_int"))
        instr.opcode = Opcode_FLOAT_TO_INT;
      else if(cstr_match(mnemonic, "int_to_float"))
        instr.opcode = Opcode_INT_TO_FLOAT;
      else
      {
        error("invalid instruction: %s", mnemonic);
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
            else
            {
              error("label must begin with a letter : %s", components[1]);
              return false;
            }
          } break;

          case Opcode_PUSH_CHAR:
          case Opcode_PUSH_INT:
          case Opcode_STORE:
          case Opcode_LOAD:
          case Opcode_GROW:
          {
            if(cstr_to_int(components[1], &instr.param.int_val))
              instr.param_type = ParamType_Int32;
            else
            {
              error("not an integer number `%s`", components[1]);
              return false;
            }
          } break;

          case Opcode_PUSH_FLOAT:
          {
            if(cstr_to_float(components[1], &instr.param.float_val))
              instr.param_type = ParamType_Float32;
            else
            {
              error("not a float number `%s`", components[1]);
              return false;
            }
          } break;

          case Opcode_PUSH_REG:
          case Opcode_POP_REG:
          {
            instr.param_type = ParamType_Reg;
            char* reg = components[1];

            if(cstr_match(reg, "sp"))
              instr.param.reg = RegName_SP;
            else if(cstr_match(reg, "fp"))
              instr.param.reg = RegName_FP;
            else if(cstr_match(reg, "ip"))
              instr.param.reg = RegName_IP;
            else
            {
              error("invalid register '%s'", components[1]);
              return false;
            }
          } break;

          case Opcode_NEW:
          {
            if(cstr_to_int(components[1], &instr.param.int_val))
              instr.param_type = ParamType_Int32;
            else
            {
              error("not an integer number '%s'", components[1]);
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
              else
              {
                error("duplicate label declaration '%s'", label.string);
                return false;
              }
            }
            else
            {
              error("label '%s' does not begin with a letter", components[1]);
              return false;
            }
          } break;

          default:
            error("incorrect number of parameters to instruction '%s'", mnemonic);
            return false;
        }
      }

      vm_program->instructions[instr_address] = instr;
    }
    else
    {
      error("illegal number of instruction components");
      return false;
    }
  }

  for(int labelIndex = 0; labelIndex < source->labesl_count; labelIndex++)
  {
    Label* label = &source->labels[labelIndex];
    int target_instr_address = label->instr_address+1;
    for(; target_instr_address < vm_program->instr_count; target_instr_address++)
    {
      Instruction* instr = &vm_program->instructions[target_instr_address];
      if(instr->opcode != Opcode_LABEL)
        break;
    }
    if(target_instr_address < vm_program->instr_count)
    {
      Instruction* label_instr = &vm_program->instructions[label->instr_address];
      label_instr->param_type = ParamType_Int32;
      label_instr->param.int_val = target_instr_address;
      label->instr_address = target_instr_address;
    }
    else
    {
      error("could not find a non-label instruction following the label %s", label->string);
      return false;
    }
  }

  for(int instr_address = 0; instr_address < vm_program->instr_count; instr_address++)
  {
    Instruction* instr = &vm_program->instructions[instr_address];
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
        instr->param.int_val = label->instr_address;
      }
      else
      {
        error("label '%s' not found", instr->param.str);
        return false;
      }
    }
  }

  return true;
}

boole
convert_hasm_to_instructions(char* text, VmProgram* vm_program)
{
  SourceProgram source = {0};
  source.text = text;
  process_source_lines(&source);

  return build_instructions(&source, vm_program);
}