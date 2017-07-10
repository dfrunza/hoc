#include "hasm.h"
#include "hocc.h"

// User-defined PE resource:
//   nameId  typeId  fileName
#define OUT_RC "CODE  VM  \"%s\""

typedef struct
{
  char* name;
  int len;
}
FileName;

typedef struct
{
  char strings[4*80 + 4*10];
  FileName ir;
  FileName irc;
  FileName rc;
  FileName res;
}
OutFileNames;

typedef struct
{
  String text;
  int text_len;
  List instr_list;
  bool success;
}
VmProgram;

#define ARENA_SIZE (2*MEGABYTE)
#define DBG_ARENA_SIZE (ARENA_SIZE/2)

internal bool debug_enabled = true;
MemoryArena* arena = 0;
MemoryArena* dbg_arena = 0;

bool
compile_error(SourceLocation* src_loc, char* file, int line, char* message, ...)
{
  va_list args;

  fprintf(stderr, "%s(%d) : (%s:%d) ", src_loc->file_path, src_loc->line_nr,
          path_make_stem(file), line);

  va_start(args, message);
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);

  return false;
}

void
DEBUG_print_sizeof_ast_structs()
{
  typedef struct 
  {
    AstNodeKind kind;
    int size;
  } StructInfo;

#define make_struct_info(KIND, STRUCT) \
  struct_info[KIND].kind = KIND; \
  struct_info[KIND].size = sizeof(STRUCT); \

#define make_zero_struct_info(KIND) \
  struct_info[KIND].kind = KIND; \
  struct_info[KIND].size = 0; \

  internal StructInfo struct_info[AstNodeKind__Count] = {0};
  assert(AstNodeKind__Null == 0);
  make_zero_struct_info(AstNodeKind__Null);
  make_struct_info(AstNodeKind_BinExpr, AstBinExpr);
  make_struct_info(AstNodeKind_UnrExpr, AstUnrExpr);
  make_struct_info(AstNodeKind_Literal, AstLiteral);
  make_struct_info(AstNodeKind_VarDecl, AstVarDecl);
  make_struct_info(AstNodeKind_VarOccur, AstVarOccur);
  make_struct_info(AstNodeKind_Block, AstBlock);
  make_struct_info(AstNodeKind_Proc, AstProc);
  make_struct_info(AstNodeKind_Id, AstId);
  make_struct_info(AstNodeKind_WhileStmt, AstWhileStmt);
  make_struct_info(AstNodeKind_ForStmt, AstForStmt);
  make_struct_info(AstNodeKind_IfStmt, AstIfStmt);
  make_struct_info(AstNodeKind_ReturnStmt, AstReturnStmt);
  make_zero_struct_info(AstNodeKind_BreakStmt);
  make_zero_struct_info(AstNodeKind_ContinueStmt);
  make_struct_info(AstNodeKind_GotoStmt, AstGotoStmt);
  make_struct_info(AstNodeKind_Label, AstLabel);
  make_struct_info(AstNodeKind_IncludeStmt, AstIncludeStmt);
  make_struct_info(AstNodeKind_Module, AstModule);
  make_struct_info(AstNodeKind_Cast, AstCast);
  make_struct_info(AstNodeKind_Call, AstCall);
  make_struct_info(AstNodeKind_Array, AstArray);
  make_struct_info(AstNodeKind_Pointer, AstPointer);
  make_struct_info(AstNodeKind_Struct, AstStruct);
  make_struct_info(AstNodeKind_Union, AstUnion);
  make_struct_info(AstNodeKind_Enum, AstEnum);
  make_struct_info(AstNodeKind_Initializer, AstInitializer);
  make_zero_struct_info(AstNodeKind_EmptyStmt);

#undef make_struct_info
#undef make_zero_size_info

#if 1
  // sort the array
  for(int i = 1; i < AstNodeKind__Count; i++)
  {
    for(int j = i;
        struct_info[j].size < struct_info[j-1].size;
        j--)
    {
      StructInfo value_at_j = struct_info[j];
      struct_info[j] = struct_info[j-1];
      struct_info[j-1] = value_at_j;
    }
  }
#endif

  printf("AstNode.size = %d bytes\n", sizeof(AstNode));
  for(int i = AstNodeKind__Count-1; i >= 0; i--)
  {
    StructInfo* info = &struct_info[i];
    printf("%s.size = %d bytes\n", get_ast_kind_printstr(info->kind), info->size);
  }
}

VmProgram*
translate(char* file_path, char* hoc_text)
{
  VmProgram* vm_program = mem_push_struct(arena, VmProgram);
  list_init(&vm_program->instr_list);
  vm_program->success = false;

  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  AstNode* ast = 0;
  if(vm_program->success = parse(&token_stream, &ast))
  {
    if(debug_enabled)
    {
      DEBUG_arena_print_occupancy("Post syntactic analysis", arena);

      String str = {0};
      str_init(&str, dbg_arena);
      DEBUG_print_ast_node(&str, 0, ast, 0);
      str_dump_to_file(&str, "out_syntax.txt");
      arena_free(dbg_arena);
    }
#if 1
    if(vm_program->success = semantic_analysis(ast))
    {
      if(debug_enabled)
      {
        DEBUG_arena_print_occupancy("Post semantic analysis", arena);

        String str = {0};
        str_init(&str, dbg_arena);
        DEBUG_print_ast_node(&str, 0, ast, 0);
        str_dump_to_file(&str, "out_semantic.txt");
        arena_free(dbg_arena);
      }
    }
#else
    assert(symbol_table.scope_id == 0);
    assert(symbol_table.nesting_depth == 0);

    if(typecheck_module(arena, &type_tuples, ast))
    {
      DEBUG_arena_print_occupancy("Typecheck", arena);

      build_module(arena, &symbol_table, ast);
      DEBUG_arena_print_occupancy("Runtime objects", arena);

      AstModule* module = &ast->module;
      if(module->main_call)
      {
        gen_module(arena, &vm_program->instr_list, module);
        DEBUG_arena_print_occupancy("Gen code", arena);

        str_init(&vm_program->text, arena);
        print_code(vm_program);
        DEBUG_arena_print_occupancy("Print code", arena);
      }
      else
        error("Missing main() procedure");
    }
#endif
  }
  return vm_program;
}

bool
make_file_names(OutFileNames* out_files, char* stem)
{
  int stem_len = cstr_len(stem);
  assert(stem_len > 0);
  bool success = (stem_len > 0 && stem_len < 80);

  if(success)
  {
    char* str = out_files->strings;

    sprintf(str, "%s.ir", stem);
    out_files->ir.name = str;
    out_files->ir.len = cstr_len(out_files->ir.name);
    str = out_files->ir.name + out_files->ir.len + 1;

    sprintf(str, "%s.irc", stem);
    out_files->irc.name = str;
    out_files->irc.len = cstr_len(out_files->irc.name);
    str = out_files->irc.name + out_files->irc.len + 1;

    sprintf(str, "%s.rc", stem);
    out_files->rc.name = str;
    out_files->rc.len = cstr_len(out_files->rc.name);
    str = out_files->rc.name + out_files->rc.len + 1;

    sprintf(str, "%s.res", stem);
    out_files->res.name = str;
    out_files->res.len = cstr_len(out_files->res.name);
  } else
    error("Length of file name out of range : '%s'", stem);
  return success;
}

bool
write_res_file(OutFileNames* out_files)
{
  char buf[200];
  sprintf(buf, OUT_RC, out_files->irc.name);
  int text_len = cstr_len(buf);
  int bytes_written = file_write_bytes(out_files->rc.name, (uint8*)buf, text_len);
  bool success = (bytes_written == text_len);
  if(success)
  {
    STARTUPINFO start_info = {0};
    start_info.dwFlags = STARTF_USESTDHANDLES;
    start_info.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    start_info.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    start_info.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    PROCESS_INFORMATION proc_info = {0};
    sprintf(buf, "rc.exe /nologo /fo%s %s", out_files->res.name, out_files->rc.name);
    DWORD exit_code = 0;
    success = CreateProcess(0, buf, 0, 0, true, 0, 0, 0, &start_info, &proc_info);
    if(success)
    {
      WaitForSingleObject(proc_info.hProcess, INFINITE);
      GetExitCodeProcess(proc_info.hProcess, &exit_code);
      success = (exit_code == 0);

      CloseHandle(proc_info.hProcess);
      CloseHandle(proc_info.hThread);
    } else {
      error("Process could not be launched : %s", buf);
      success = false;
    }
  } else {
    error("RC file '%s' incompletely written", out_files->rc.name);
    success = false;
  }
  return success;
}

bool
write_ir_file(OutFileNames* out_files, VmProgram* vm_program)
{
  int bytes_written = file_write_bytes(out_files->ir.name, (uint8*)vm_program->text.head, vm_program->text_len);
  bool success = (bytes_written == vm_program->text_len);
  if(!success)
    error("IR file '%s' incompletely written", out_files->ir.name);
  return success;
}

bool
write_irc_file(OutFileNames* out_files, HasmCode* hasm_code)
{
  size_t bytes_written = file_write_bytes(out_files->irc.name, hasm_code->code_start, hasm_code->code_size);
  bool success = (bytes_written == hasm_code->code_size);
  if(!success)
    error("IRC file '%s' incompletely written", out_files->irc.name);
  return success;
}

int
main(int argc, char* argv[])
{
  int ret = -1; // error

  if(argc >= 2)
  {
    arena = arena_new(ARENA_SIZE);
    if(debug_enabled)
    {
      dbg_arena = arena_push(arena, DBG_ARENA_SIZE);
      DEBUG_print_sizeof_ast_structs();
    }

    char* file_path = argv[1];
    char* hoc_text = file_read_text(arena, file_path);
    DEBUG_arena_print_occupancy("Read HoC text", arena);

    if(hoc_text)
    {
      VmProgram* vm_program = translate(file_path, hoc_text);
      if(vm_program->success)
      {
#if 0
        OutFileNames out_files = {0};
        char* file_stem = path_make_stem(file_path);

        bool success = make_file_names(&out_files, file_stem) &&
          write_ir_file(&out_files, vm_program);

        if(success)
        {
          HasmCode* hasm_code = 0;
          char* hasm_text = vm_program->text.head;
          bool success = translate_ir_to_code(&arena, hasm_text, &hasm_code);

          if(success)
          {
            if(success = write_irc_file(&out_files, hasm_code) && write_res_file(&out_files))
              ret = 0;
            else
              error("Could not write HASM and RES file: `%s`, `%s`", out_files.irc.name, out_files.res.name);
          }
        }
        else
          error("Could not write file `%s`", out_files.ir.name);
#endif
      }
      else
        error("Program could not be translated");
    }
    else
      error("Could not read file `%s`", file_path);
  }
  else
    error("Missing argument: input source file");

#if 0
  getc(stdin);
#endif
  return ret;
}
