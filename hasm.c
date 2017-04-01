#include "lib.c"
#include "ir.h"

typedef struct
{
  char* string;
  int instrAddress;
}
Label;

typedef struct
{
  char* text;

  int maxLines;
  int lineCount;
  InstructionLine lines[1024];

  int maxLabels;
  int labelCount;
  Label labels[512]; //FIXME: Unchecked bounds
}
SourceProgram;

int break_instr_into_components(char* str, char* components[], int maxComponentCount)
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

int find_instr_address_at_line(SourceProgram* source, int lineNr)
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

Label* find_label(SourceProgram* source, char* labelString)
{
  Label* result = 0;
  for(int i = 0; i < source->labelCount; i++)
  {
    Label* label = &source->labels[i];
    if(str_equals(label->string, labelString))
    {
      result = label;
      break;
    }
  }
  return result;
}

bool32 is_valid_label(char *label)
{
  char startChar = *label;
  return ('A' <= startChar && startChar <= 'Z') || ('a' <= startChar && startChar <= 'z') || startChar == '.';
}

void process_source_lines(SourceProgram* source)
{/*>>>*/
  source->lineCount = 0;
  char* charPtr = &source->text[0];
  if(*charPtr != '\0')
  {
    int sourceLineNr = 1;

    if(*charPtr != '\n' && *charPtr != ';')
    {
      InstructionLine instrLine = {0};
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
            memset(&instrLine, 0, sizeof(instrLine));
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

bool32 build_ir_code(MemoryArena* arena, SourceProgram* source, IrCode** out_code)
{/*>>>*/
  IrCode* code = push_element(arena, IrCode, 1);
  copy_cstr(code->groove, "IRC");
  code->codeStart = (uint8*)code;
  code->instrCount = source->lineCount;
  code->instrArray = push_element(arena, Instruction, code->instrCount);
  code->codeSize = (int)((uint8*)arena->free - code->codeStart);
  *out_code = code;

  for(int instrAddress = 0; instrAddress < code->instrCount; instrAddress++)
  {
    Instruction instr = {0};
    InstructionLine* instrLine = &source->lines[instrAddress];
    instr.sourceLineNr = instrLine->sourceLineNr;

    char* components[2] = {0};
    int componentCount = break_instr_into_components(instrLine->string,
                                                     components, sizeof_array(components));
    if(componentCount >= 1 && componentCount <= sizeof_array(components))
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

      if(componentCount == 2)
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
                label.instrAddress = instrAddress;

                if(!find_label(source, label.string))
                  source->labels[source->labelCount++] = label;
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

      code->instrArray[instrAddress] = instr;
    }
    else
    {
      error("Illegal number of instruction components");
      return false;
    }
  }

  for(int labelIndex = 0; labelIndex < source->labelCount; labelIndex++)
  {
    Label* label = &source->labels[labelIndex];
    int targetInstrAddress = label->instrAddress+1;
    for(; targetInstrAddress < code->instrCount; targetInstrAddress++)
    {
      Instruction* instr = &code->instrArray[targetInstrAddress];
      if(instr->opcode != Opcode_LABEL)
        break;
    }
    if(targetInstrAddress < code->instrCount)
    {
      Instruction* labelInstr = &code->instrArray[label->instrAddress];
      labelInstr->param_type = ParamType_Int32;
      labelInstr->param.int_num = targetInstrAddress;
      label->instrAddress = targetInstrAddress;
    }
    else {
      error("Could not find a non-label instruction following the label %s", label->string);
      return false;
    }
  }

  for(int instrAddress = 0; instrAddress < code->instrCount; instrAddress++)
  {
    Instruction* instr = &code->instrArray[instrAddress];
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
        instr->param.int_num = label->instrAddress;
      }
      else {
        error("Label '%s' not found", instr->param.str);
        return false;
      }
    }
  }

  return true;
}/*<<<*/

bool32 translate_ir_to_code(MemoryArena* arena, char* text, IrCode** code)
{
  bool32 success = true;

  SourceProgram source = {0};
  source.text = text;
  process_source_lines(&source);

  success = build_ir_code(arena, &source, code);
  return success;
}
