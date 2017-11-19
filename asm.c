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

int instr_to_components(char* str, char* components[], int max_component_count)
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
#endif

Label* find_label(SourceProgram* source, char* label_string)
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

bool is_valid_label(char *label)
{
  char start_char = *label;
  return ('A' <= start_char && start_char <= 'Z') || ('a' <= start_char && start_char <= 'z') || start_char == '.';
}

void process_source_lines(SourceProgram* source)
{
  source->line_count = 0;
  char* char_ptr = &source->text[0];
  if(*char_ptr)
  {
    int source_line_nr = 1;

    if(!cstr_contains_char("\r\n", *char_ptr))
    {
      InstructionLine instr_line = {0};
      instr_line.source_line_nr = source_line_nr;
      instr_line.string = char_ptr;
      source->lines[source->line_count++] = instr_line;

      char_ptr++;
      while(*char_ptr)
      {
        if(cstr_contains_char("\r\n", *char_ptr))
        {
          source_line_nr++;

          while(cstr_contains_char(" \r\n", *char_ptr))
          {
            *char_ptr = '\0';
            char_ptr++;
          }

          if(*char_ptr)
          {
            mem_zero_struct(&instr_line, InstructionLine);
            instr_line.source_line_nr = source_line_nr;
            instr_line.string = char_ptr;
            source->lines[source->line_count++] = instr_line;
          }
        }
        char_ptr++;
      }
    }
  }
}

bool build_instructions(SourceProgram* source, VmProgram* vm_program)
{
  vm_program->instr_count = source->line_count;
  vm_program->instructions = mem_push_array_nz(arena, Instruction, vm_program->instr_count);

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
      {
        instr.opcode = eOpcode_POP_REG;
      }
      else if(cstr_match(mnemonic, "push_int8"))
      {
        instr.opcode = eOpcode_PUSH_INT8;
      }
      else if(cstr_match(mnemonic, "push_int32"))
      {
        instr.opcode = eOpcode_PUSH_INT32;
      }
      else if(cstr_match(mnemonic, "push_float32"))
      {
        instr.opcode = eOpcode_PUSH_FLOAT32;
      }
      else if(cstr_match(mnemonic, "push_reg"))
      {
        instr.opcode = eOpcode_PUSH_REG;
      }
      else if(cstr_match(mnemonic, "store"))
      {
        instr.opcode = eOpcode_STORE;
      }
      else if(cstr_match(mnemonic, "load"))
      {
        instr.opcode = eOpcode_LOAD;
      }
      else if(cstr_match(mnemonic, "grow"))
      {
        instr.opcode = eOpcode_GROW;
      }
      else if(cstr_match(mnemonic, "grownz"))
      {
        instr.opcode = eOpcode_GROWNZ;
      }
      else if(cstr_match(mnemonic, "new"))
      {
        instr.opcode = eOpcode_NEW;
      }
      else if(cstr_match(mnemonic, "add_int32"))
      {
        instr.opcode = eOpcode_ADD_INT32;
      }
      else if(cstr_match(mnemonic, "sub_int32"))
      {
        instr.opcode = eOpcode_SUB_INT32;
      }
      else if(cstr_match(mnemonic, "mul_int32"))
      {
        instr.opcode = eOpcode_MUL_INT32;
      }
      else if(cstr_match(mnemonic, "div_int32"))
      {
        instr.opcode = eOpcode_DIV_INT32;
      }
      else if(cstr_match(mnemonic, "mod_int32"))
      {
        instr.opcode = eOpcode_MOD_INT32;
      }
      else if(cstr_match(mnemonic, "add_float32"))
      {
        instr.opcode = eOpcode_ADD_FLOAT32;
      }
      else if(cstr_match(mnemonic, "sub_float32"))
      {
        instr.opcode = eOpcode_SUB_FLOAT32;
      }
      else if(cstr_match(mnemonic, "mul_float32"))
      {
        instr.opcode = eOpcode_MUL_FLOAT32;
      }
      else if(cstr_match(mnemonic, "div_float32"))
      {
        instr.opcode = eOpcode_DIV_FLOAT32;
      }
      else if(cstr_match(mnemonic, "neg_float32"))
      {
        instr.opcode = eOpcode_NEG_FLOAT32;
      }
      else if(cstr_match(mnemonic, "incr_int32"))
      {
        instr.opcode = eOpcode_INCR_INT32;
      }
      else if(cstr_match(mnemonic, "decr_int32"))
      {
        instr.opcode = eOpcode_DECR_INT32;
      }
      else if(cstr_match(mnemonic, "halt"))
      {
        instr.opcode = eOpcode_HALT;
      }
      else if(cstr_match(mnemonic, "putc"))
      {
        instr.opcode = eOpcode_PUTC;
      }
      else if(cstr_match(mnemonic, "dup"))
      {
        instr.opcode = eOpcode_DUP;
      }
      else if(cstr_match(mnemonic, "goto"))
      {
        instr.opcode = eOpcode_GOTO;
      }
      else if(cstr_match(mnemonic, "jumpnz"))
      {
        instr.opcode = eOpcode_JUMPNZ;
      }
      else if(cstr_match(mnemonic, "jumpz"))
      {
        instr.opcode = eOpcode_JUMPZ;
      }
      else if(cstr_match(mnemonic, "cmpeq_int8"))
      {
        instr.opcode = eOpcode_CMPEQ_INT8;
      }
      else if(cstr_match(mnemonic, "cmpneq_int8"))
      {
        instr.opcode = eOpcode_CMPNEQ_INT8;
      }
      else if(cstr_match(mnemonic, "cmplss_int8"))
      {
        instr.opcode = eOpcode_CMPLSS_INT8;
      }
      else if(cstr_match(mnemonic, "cmpgrt_int8"))
      {
        instr.opcode = eOpcode_CMPGRT_INT8;
      }
      else if(cstr_match(mnemonic, "cmpeq_int32"))
      {
        instr.opcode = eOpcode_CMPEQ_INT32;
      }
      else if(cstr_match(mnemonic, "cmpneq_int32"))
      {
        instr.opcode = eOpcode_CMPNEQ_INT32;
      }
      else if(cstr_match(mnemonic, "cmplss_int32"))
      {
        instr.opcode = eOpcode_CMPLSS_INT32;
      }
      else if(cstr_match(mnemonic, "cmpgrt_int32"))
      {
        instr.opcode = eOpcode_CMPGRT_FLOAT32;
      }
      else if(cstr_match(mnemonic, "cmpeq_float32"))
      {
        instr.opcode = eOpcode_CMPEQ_FLOAT32;
      }
      else if(cstr_match(mnemonic, "cmpneq_float32"))
      {
        instr.opcode = eOpcode_CMPNEQ_FLOAT32;
      }
      else if(cstr_match(mnemonic, "cmplss_float32"))
      {
        instr.opcode = eOpcode_CMPLSS_FLOAT32;
      }
      else if(cstr_match(mnemonic, "cmpgrt_float32"))
      {
        instr.opcode = eOpcode_CMPGRT_FLOAT32;
      }
      else if(cstr_match(mnemonic, "and"))
      {
        instr.opcode = eOpcode_AND;
      }
      else if(cstr_match(mnemonic, "or"))
      {
        instr.opcode = eOpcode_OR;
      }
      else if(cstr_match(mnemonic, "not"))
      {
        instr.opcode = eOpcode_NOT;
      }
      else if(cstr_match(mnemonic, "neg_int32"))
      {
        instr.opcode = eOpcode_NEG_INT32;
      }
      else if(cstr_match(mnemonic, "neg_float32"))
      {
        instr.opcode = eOpcode_NEG_FLOAT32;
      }
      else if(cstr_match(mnemonic, "label"))
      {
        instr.opcode = eOpcode_LABEL;
      }
      else if(cstr_match(mnemonic, "noop"))
      {
        instr.opcode = eOpcode_NOOP;
      }
      else if(cstr_match(mnemonic, "call"))
      {
        instr.opcode = eOpcode_CALL;
      }
      else if(cstr_match(mnemonic, "return"))
      {
        instr.opcode = eOpcode_RETURN;
      }
      else if(cstr_match(mnemonic, "enter"))
      {
        instr.opcode = eOpcode_ENTER;
      }
      else if(cstr_match(mnemonic, "leave"))
      {
        instr.opcode = eOpcode_LEAVE;
      }
      else if(cstr_match(mnemonic, "float32_to_int32"))
      {
        instr.opcode = eOpcode_FLOAT32_TO_INT32;
      }
      else if(cstr_match(mnemonic, "int32_to_float32"))
      {
        instr.opcode = eOpcode_INT32_TO_FLOAT32;
      }
      else
      {
        error("invalid instruction: %s", mnemonic);
        return false;
      }

      if(component_count == 2)
      {
        switch(instr.opcode)
        {
          case eOpcode_GOTO:
          case eOpcode_JUMPNZ:
          case eOpcode_JUMPZ:
          case eOpcode_CALL:
          {
            if(is_valid_label(components[1]))
            {
              instr.param_type = eParamType_str;
              instr.param.str = components[1];
            }
            else
            {
              error("label must begin with a letter : %s", components[1]);
              return false;
            }
          } break;

          case eOpcode_PUSH_INT8:
          {
            if(cstr_to_int(components[1], &instr.param.int_val))
            {
              instr.param_type = eParamType_int8;
            }
            else
            {
              error("not an integer number `%s`", components[1]);
              return false;
            }
          } break;

          case eOpcode_PUSH_INT32:
          case eOpcode_STORE:
          case eOpcode_LOAD:
          case eOpcode_GROW:
          case eOpcode_GROWNZ:
          {
            if(cstr_to_int(components[1], &instr.param.int_val))
            {
              instr.param_type = eParamType_int32;
            }
            else
            {
              error("not an integer number `%s`", components[1]);
              return false;
            }
          } break;

          case eOpcode_PUSH_FLOAT32:
          {
            if(cstr_to_float(components[1], &instr.param.float_val))
            {
              instr.param_type = eParamType_float32;
            }
            else
            {
              error("not a float number `%s`", components[1]);
              return false;
            }
          } break;

          case eOpcode_PUSH_REG:
          case eOpcode_POP_REG:
          {
            instr.param_type = eParamType_reg;
            char* reg = components[1];

            if(cstr_match(reg, "sp"))
              instr.param.reg = eRegName_SP;
            else if(cstr_match(reg, "fp"))
              instr.param.reg = eRegName_FP;
            else if(cstr_match(reg, "ip"))
              instr.param.reg = eRegName_IP;
            else
            {
              error("invalid register '%s'", components[1]);
              return false;
            }
          } break;

          case eOpcode_NEW:
          {
            if(cstr_to_int(components[1], &instr.param.int_val))
              instr.param_type = eParamType_int32;
            else
            {
              error("not an integer number '%s'", components[1]);
              return false;
            }
          } break;

          case eOpcode_LABEL:
          {
            if(is_valid_label(components[1]))
            {
              instr.param_type = eParamType_str;
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
      if(instr->opcode != eOpcode_LABEL)
        break;
    }
    if(target_instr_address < vm_program->instr_count)
    {
      Instruction* label_instr = &vm_program->instructions[label->instr_address];
      label_instr->param_type = eParamType_int32;
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
    if(instr->opcode == eOpcode_GOTO ||
       instr->opcode == eOpcode_JUMPNZ ||
       instr->opcode == eOpcode_JUMPZ ||
       instr->opcode == eOpcode_CALL)
    {
      assert(instr->param_type == eParamType_str);
      Label* label = find_label(source, instr->param.str);
      if(label)
      {
        instr->param_type = eParamType_int32;
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

bool convert_hasm_to_instructions(VmProgram* vm_program)
{
  SourceProgram source = {0};
  source.text = vm_program->text;
  process_source_lines(&source);

  return build_instructions(&source, vm_program);
}
