#include "lib.cpp"
#include "ir.h"

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

bool32 IsValidLabel(char *label)
{
  char startChar = *label;
  return ('A' <= startChar && startChar <= 'Z') || ('a' <= startChar && startChar <= 'z') || startChar == '.';
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

bool32 BuildIrCode(MemoryArena* arena, SourceProgram* source, IrCode** out_code)
{/*>>>*/
  IrCode* code = PushElement(arena, IrCode, 1);
  CopyStr(code->groove, "IRC");
  code->codeStart = (uint8*)code;
  code->instrCount = source->lineCount;
  code->instrArray = PushElement(arena, Instruction, code->instrCount);
  code->codeSize = (int)((uint8*)arena->free - code->codeStart);
  *out_code = code;

  for(int instrAddress = 0; instrAddress < code->instrCount; instrAddress++)
  {
    Instruction instr = {};
    InstructionLine* instrLine = &source->lines[instrAddress];
    instr.sourceLineNr = instrLine->sourceLineNr;

    char* components[2] = {};
    int componentCount = BreakInstructionIntoComponents(instrLine->string,
                                                        components, SizeofArray(components));
    if(componentCount >= 1 && componentCount <= SizeofArray(components))
    {
      char* mnemonic = components[0];

      if(StrEquals(mnemonic, "pop"))
        instr.opcode = Opcode::POP;
      else if(StrEquals(mnemonic, "push"))
        instr.opcode = Opcode::PUSH;
      else if(StrEquals(mnemonic, "store"))
        instr.opcode = Opcode::STORE;
      else if(StrEquals(mnemonic, "store8"))
        instr.opcode = Opcode::STORE8;
      else if(StrEquals(mnemonic, "load"))
        instr.opcode = Opcode::LOAD;
      else if(StrEquals(mnemonic, "load8"))
        instr.opcode = Opcode::LOAD8;
      else if(StrEquals(mnemonic, "add"))
        instr.opcode = Opcode::ADD;
      else if(StrEquals(mnemonic, "sub"))
        instr.opcode = Opcode::SUB;
      else if(StrEquals(mnemonic, "mul"))
        instr.opcode = Opcode::MUL;
      else if(StrEquals(mnemonic, "div"))
        instr.opcode = Opcode::DIV;
      else if(StrEquals(mnemonic, "mod"))
        instr.opcode = Opcode::MOD;
      else if(StrEquals(mnemonic, "incr"))
        instr.opcode = Opcode::INCR;
      else if(StrEquals(mnemonic, "decr"))
        instr.opcode = Opcode::DECR;
      else if(StrEquals(mnemonic, "halt"))
        instr.opcode = Opcode::HALT;
      else if(StrEquals(mnemonic, "print"))
        instr.opcode = Opcode::PRINT;
      else if(StrEquals(mnemonic, "dup"))
        instr.opcode = Opcode::DUP;
      else if(StrEquals(mnemonic, "goto"))
        instr.opcode = Opcode::GOTO;
      else if(StrEquals(mnemonic, "jumpnz"))
        instr.opcode = Opcode::JUMPNZ;
      else if(StrEquals(mnemonic, "jumpz"))
        instr.opcode = Opcode::JUMPZ;
      else if(StrEquals(mnemonic, "label"))
        instr.opcode = Opcode::LABEL;
      else if(StrEquals(mnemonic, "noop"))
        instr.opcode = Opcode::NOOP;
      else if(StrEquals(mnemonic, "call"))
        instr.opcode = Opcode::CALL;
      else if(StrEquals(mnemonic, "return"))
        instr.opcode = Opcode::RETURN;
      else if(StrEquals(mnemonic, "enter"))
        instr.opcode = Opcode::ENTER;
      else if(StrEquals(mnemonic, "leave"))
        instr.opcode = Opcode::LEAVE;
      else if(StrEquals(mnemonic, "alloc"))
        instr.opcode = Opcode::ALLOC;
      else {
        Error("Invalid instruction: %s", mnemonic);
        return false;
      }

      if(componentCount == 2)
      {
        switch(instr.opcode)
        {
          case Opcode::GOTO:
          case Opcode::JUMPNZ:
          case Opcode::JUMPZ:
          case Opcode::CALL:
            {
              if(IsValidLabel(components[1]))
              {
                instr.paramType = ParamType::String;
                instr.param.str = components[1];
              }
              else {
                Error("Label must begin with a letter : %s", components[1]);
                return false;
              }
            } break;

          case Opcode::POP:
          case Opcode::PUSH:
            {
              if(StrToInt(components[1], &instr.param.intNum))
                instr.paramType = ParamType::Int32;
              else {
                instr.paramType = ParamType::Reg;
                char* reg = components[1];

                if(StrMatch(reg, "sp"))
                  instr.param.reg = RegName::SP;
                else if(StrMatch(reg, "fp"))
                  instr.param.reg = RegName::FP;
                else if(StrMatch(reg, "ip"))
                  instr.param.reg = RegName::IP;
                else {
                  Error("Invalid register '%s'", components[1]);
                  return false;
                }
              }
            } break;

          case Opcode::ALLOC:
            {
              if(StrToInt(components[1], &instr.param.intNum))
                instr.paramType = ParamType::Int32;
              else {
                Error("Invalid parameter '%s'", components[1]);
                return false;
              }
            } break;

          case Opcode::LABEL:
            {
              if(IsValidLabel(components[1]))
              {
                instr.paramType = ParamType::String;
                instr.param.str = components[1];

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

      code->instrArray[instrAddress] = instr;
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
    for(; targetInstrAddress < code->instrCount; targetInstrAddress++)
    {
      Instruction* instr = &code->instrArray[targetInstrAddress];
      if(instr->opcode != Opcode::LABEL)
        break;
    }
    if(targetInstrAddress < code->instrCount)
    {
      Instruction* labelInstr = &code->instrArray[label->instrAddress];
      labelInstr->paramType = ParamType::Int32;
      labelInstr->param.intNum = targetInstrAddress;
      label->instrAddress = targetInstrAddress;
    }
    else {
      Error("Could not find a non-label instruction following the label %s", label->string);
      return false;
    }
  }

  for(int instrAddress = 0; instrAddress < code->instrCount; instrAddress++)
  {
    Instruction* instr = &code->instrArray[instrAddress];
    if(instr->opcode == Opcode::GOTO ||
       instr->opcode == Opcode::JUMPNZ ||
       instr->opcode == Opcode::JUMPZ ||
       instr->opcode == Opcode::CALL)
    {
      assert(instr->paramType == ParamType::String);
      Label* label = FindLabel(source, instr->param.str);
      if(label)
      {
        instr->paramType = ParamType::Int32;
        instr->param.intNum = label->instrAddress;
      }
      else {
        Error("Label '%s' not found", instr->param.str);
        return false;
      }
    }
  }

  return true;
}/*<<<*/

bool32 TranslateIrToCode(MemoryArena* arena, char* text, IrCode** code)
{
  bool32 success = true;

  SourceProgram source = {};
  source.text = text;
  ProcessSourceLines(&source);

  success = BuildIrCode(arena, &source, code);
  return success;
}
