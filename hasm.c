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

bool32 BuildIrCode(MemoryArena* arena, SourceProgram* source, IrCode** out_code)
{/*>>>*/
  IrCode* code = PushElement(arena, IrCode, 1);
  CopyCStr(code->groove, "IRC");
  code->codeStart = (uint8*)code;
  code->instrCount = source->lineCount;
  code->instrArray = PushElement(arena, Instruction, code->instrCount);
  code->codeSize = (int)((uint8*)arena->free - code->codeStart);
  *out_code = code;

  for(int instrAddress = 0; instrAddress < code->instrCount; instrAddress++)
  {
    Instruction instr = {0};
    InstructionLine* instrLine = &source->lines[instrAddress];
    instr.sourceLineNr = instrLine->sourceLineNr;

    char* components[2] = {0};
    int componentCount = BreakInstructionIntoComponents(instrLine->string,
                                                        components, SizeofArray(components));
    if(componentCount >= 1 && componentCount <= SizeofArray(components))
    {
      char* mnemonic = components[0];

      if(StrEquals(mnemonic, "pop"))
        instr.opcode = Opcode_POP;
      else if(StrEquals(mnemonic, "push"))
        instr.opcode = Opcode_PUSH;
      else if(StrEquals(mnemonic, "store"))
        instr.opcode = Opcode_STORE;
      else if(StrEquals(mnemonic, "store8"))
        instr.opcode = Opcode_STORE8;
      else if(StrEquals(mnemonic, "load"))
        instr.opcode = Opcode_LOAD;
      else if(StrEquals(mnemonic, "load8"))
        instr.opcode = Opcode_LOAD8;
      else if(StrEquals(mnemonic, "add"))
        instr.opcode = Opcode_ADD;
      else if(StrEquals(mnemonic, "sub"))
        instr.opcode = Opcode_SUB;
      else if(StrEquals(mnemonic, "mul"))
        instr.opcode = Opcode_MUL;
      else if(StrEquals(mnemonic, "div"))
        instr.opcode = Opcode_DIV;
      else if(StrEquals(mnemonic, "mod"))
        instr.opcode = Opcode_MOD;
      else if(StrEquals(mnemonic, "incr"))
        instr.opcode = Opcode_INCR;
      else if(StrEquals(mnemonic, "decr"))
        instr.opcode = Opcode_DECR;
      else if(StrEquals(mnemonic, "halt"))
        instr.opcode = Opcode_HALT;
      else if(StrEquals(mnemonic, "print"))
        instr.opcode = Opcode_PRINT;
      else if(StrEquals(mnemonic, "dup"))
        instr.opcode = Opcode_DUP;
      else if(StrEquals(mnemonic, "goto"))
        instr.opcode = Opcode_GOTO;
      else if(StrEquals(mnemonic, "jumpnz"))
        instr.opcode = Opcode_JUMPNZ;
      else if(StrEquals(mnemonic, "jumpz"))
        instr.opcode = Opcode_JUMPZ;
      else if(StrEquals(mnemonic, "label"))
        instr.opcode = Opcode_LABEL;
      else if(StrEquals(mnemonic, "noop"))
        instr.opcode = Opcode_NOOP;
      else if(StrEquals(mnemonic, "call"))
        instr.opcode = Opcode_CALL;
      else if(StrEquals(mnemonic, "return"))
        instr.opcode = Opcode_RETURN;
      else if(StrEquals(mnemonic, "enter"))
        instr.opcode = Opcode_ENTER;
      else if(StrEquals(mnemonic, "leave"))
        instr.opcode = Opcode_LEAVE;
      else if(StrEquals(mnemonic, "alloc"))
        instr.opcode = Opcode_ALLOC;
      else {
        Error("Invalid instruction: %s", mnemonic);
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
              if(IsValidLabel(components[1]))
              {
                instr.paramType = ParamType_String;
                instr.param.str = components[1];
              }
              else {
                Error("Label must begin with a letter : %s", components[1]);
                return false;
              }
            } break;

          case Opcode_POP:
          case Opcode_PUSH:
            {
              if(StrToInt(components[1], &instr.param.intNum))
                instr.paramType = ParamType_Int32;
              else {
                instr.paramType = ParamType_Reg;
                char* reg = components[1];

                if(StrMatch(reg, "sp"))
                  instr.param.reg = RegName_SP;
                else if(StrMatch(reg, "fp"))
                  instr.param.reg = RegName_FP;
                else if(StrMatch(reg, "ip"))
                  instr.param.reg = RegName_IP;
                else {
                  Error("Invalid register '%s'", components[1]);
                  return false;
                }
              }
            } break;

          case Opcode_ALLOC:
            {
              if(StrToInt(components[1], &instr.param.intNum))
                instr.paramType = ParamType_Int32;
              else {
                Error("Invalid parameter '%s'", components[1]);
                return false;
              }
            } break;

          case Opcode_LABEL:
            {
              if(IsValidLabel(components[1]))
              {
                instr.paramType = ParamType_String;
                instr.param.str = components[1];

                Label label = {0};
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
      if(instr->opcode != Opcode_LABEL)
        break;
    }
    if(targetInstrAddress < code->instrCount)
    {
      Instruction* labelInstr = &code->instrArray[label->instrAddress];
      labelInstr->paramType = ParamType_Int32;
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
    if(instr->opcode == Opcode_GOTO ||
       instr->opcode == Opcode_JUMPNZ ||
       instr->opcode == Opcode_JUMPZ ||
       instr->opcode == Opcode_CALL)
    {
      assert(instr->paramType == ParamType_String);
      Label* label = FindLabel(source, instr->param.str);
      if(label)
      {
        instr->paramType = ParamType_Int32;
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

  SourceProgram source = {0};
  source.text = text;
  ProcessSourceLines(&source);

  success = BuildIrCode(arena, &source, code);
  return success;
}
