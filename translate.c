#include "lib.c"

typedef struct Symbol Symbol;
typedef struct AstNode AstNode;
typedef struct Type Type;

#include "lex.h"
//#include "typecheck.h"
#include "syntax.h"
//#include "runtime_obj.h"

typedef struct
{
  String text;
  int text_len;
  List instr_list; // <Instruction>
}
VmProgram;

void
compile_error(SourceLocation* src_loc, char* message, ...)
{
  va_list args;

  fprintf(stderr, "%s(%d) : ", src_loc->file_path, src_loc->line_nr);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
}

#include "lex.c"
//#include "typecheck.c"
#include "syntax.c"
//#include "runtime_obj.c"
//#include "codegen.c"

VmProgram* translate_hoc(MemoryArena* arena, char* file_path, char* hoc_text)
{
  bool32 success = false;

  /*
  init_global_basic_types(arena);
  list_init(&type_tuples);

  SymbolTable symbol_table = {0};
  symbol_table.arena = arena;
  add_keyword_list(arena, &symbol_table);
  */

  TokenStream token_stream = {0};
  token_stream_init(&token_stream, hoc_text, file_path);

  get_next_token(arena, &token_stream);

  AstNode* node = 0;
  success = parse(arena, &token_stream, &node);
  DEBUG_arena_print_occupancy("Parse", arena);

  VmProgram* vm_program = 0;
#if 1
  {
    String str = {0};
    str_init(&str, arena);
    DEBUG_print_ast(&str, 0, node, 0);
    str_stdout(&str);
  }
#else
  if(success)
  {
    assert(symbol_table.scope_id == 0);
    assert(symbol_table.nesting_depth == 0);

    if(typecheck_module(arena, &type_tuples, node))
    {
      DEBUG_arena_print_occupancy("Typecheck", arena);

      build_module(arena, &symbol_table, node);
      DEBUG_arena_print_occupancy("Runtime objects", arena);

      AstModule* module = &node->module;
      if(module->main_call)
      {
        vm_program = mem_push_struct(arena, VmProgram, 1);
        list_init(&vm_program->instr_list);
        gen_module(arena, &vm_program->instr_list, module);
        DEBUG_arena_print_occupancy("Gen code", arena);

        str_init(&vm_program->text, arena);
        print_code(vm_program);
        DEBUG_arena_print_occupancy("Print code", arena);
      }
      else
      {
        error("Missing main() procedure");
      }
    }
  }
#endif
  return vm_program;
}

