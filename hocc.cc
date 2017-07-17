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
  bool32 success;
}
VmProgram;

local bool32 DEBUG_enabled = true;
MemoryArena* arena = 0;
MemoryArena* DEBUG_arena = 0;
MemoryArena* sym_arena = 0;
extern SymbolTable* symtab;

void
DEBUG_print_arena_usage(char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  ArenaUsage sym_usage = arena_usage(sym_arena);
  printf("arena in_use : %.2f%% -- %s\n", usage.in_use*100, tag);
  printf("sym_arena in_use : %.2f%% -- %s\n", sym_usage.in_use*100, tag);
}

bool32
compile_error_f(char* file, int line, SourceLocation* src_loc, char* message, ...)
{
  char* filename_buf = mem_push_count_nz(arena, char, cstr_len(file));
  cstr_copy(filename_buf, file);

  if(src_loc->line_nr >= 0)
    fprintf(stderr, "%s(%d) : (%s:%d) ", src_loc->file_path, src_loc->line_nr,
            path_make_stem(filename_buf), line);
  else
    fprintf(stderr, "%s(%d) : ", file, line);

  va_list args;
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);

  fprintf(stderr, "\n");
  return false;
}

void
DEBUG_print_sizeof_ast_structs()
{
  typedef struct 
  {
    AstNodeKind kind;
    int size;
  }
  StructInfo;

#define make_struct_info(KIND, STRUCT) \
  struct_info[KIND].kind = KIND; \
  struct_info[KIND].size = sizeof(STRUCT); \

  local StructInfo struct_info[(int)AstNodeKind__Count] = {};
  assert(AstNodeKind__Null == 0);
  AstNode node;
  mem_zero_struct(&node, AstNode);
  make_struct_info(AstNodeKind_BinExpr, node.bin_expr);
  make_struct_info(AstNodeKind_UnrExpr, node.unr_expr);
  make_struct_info(AstNodeKind_Literal, node.literal);
  make_struct_info(AstNodeKind_VarDecl, node.var_decl);
//  make_struct_info(AstNodeKind_VarOccur, AstVarOccur);
  make_struct_info(AstNodeKind_Block, node.block);
  make_struct_info(AstNodeKind_Proc, node.block);
  make_struct_info(AstNodeKind_Id, node.id);
  make_struct_info(AstNodeKind_WhileStmt, node.while_stmt);
  make_struct_info(AstNodeKind_ForStmt, node.for_stmt);
  make_struct_info(AstNodeKind_IfStmt, node.if_stmt);
  make_struct_info(AstNodeKind_ReturnStmt, node.ret_stmt);
  make_struct_info(AstNodeKind_GotoStmt, node.goto_stmt);
  make_struct_info(AstNodeKind_Label, node.label);
  make_struct_info(AstNodeKind_IncludeStmt, node.incl_stmt);
  make_struct_info(AstNodeKind_Module, node.module);
  make_struct_info(AstNodeKind_Cast, node.cast);
  make_struct_info(AstNodeKind_Call, node.call);
  make_struct_info(AstNodeKind_Array, node.array);
  make_struct_info(AstNodeKind_Pointer, node.ptr);
  make_struct_info(AstNodeKind_Struct, node.struct_decl);
  make_struct_info(AstNodeKind_Union, node.union_decl);
  make_struct_info(AstNodeKind_Enum, node.enum_decl);
  make_struct_info(AstNodeKind_Initializer, node.initer);

#undef make_struct_info
#undef make_zero_size_info

#if 1
  // insertion-sort the array
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
    if(info->size > 0)
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
    if(DEBUG_enabled)
    {/*>>>*/
      DEBUG_print_arena_usage("Post syntactic analysis");

      String str = {0};
      str_init(&str, DEBUG_arena);
      DEBUG_print_ast_node(&str, 0, ast, 0);
      str_dump_to_file(&str, "out_syntax.txt");
      arena_free(DEBUG_arena);
    }/*<<<*/
#if 1
    if(vm_program->success = semantic_analysis(ast))
    {
      assert(symtab->block_id == 0);
      assert(symtab->nesting_depth == 0);

      if(DEBUG_enabled)
      {/*>>>*/
        DEBUG_print_arena_usage("Post semantic analysis");

        String str = {0};
        str_init(&str, DEBUG_arena);
        DEBUG_print_ast_node(&str, 0, ast, 0);
        str_dump_to_file(&str, "out_semantic.txt");
        arena_free(DEBUG_arena);
      }/*<<<*/

      if(vm_program->success = build_runtime(ast))
      {
        if(DEBUG_enabled)
        {/*>>>*/
          DEBUG_print_arena_usage("Post runtime build");
        }/*<<<*/
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

bool32
make_file_names(OutFileNames* out_files, char* stem)
{
  int stem_len = cstr_len(stem);
  assert(stem_len > 0);
  bool32 success = true;

  if(success = (stem_len > 0 && stem_len < 80))
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
  }
  else
    error("Length of file name out of range : '%s'", stem);
  return success;
}

bool32
write_res_file(OutFileNames* out_files)
{
  char buf[200];
  sprintf(buf, OUT_RC, out_files->irc.name);
  int text_len = cstr_len(buf);
  int bytes_written = file_write_bytes(out_files->rc.name, (uint8*)buf, text_len);
  bool32 success = (bytes_written == text_len);
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
    if(success = CreateProcess(0, buf, 0, 0, true, 0, 0, 0, &start_info, &proc_info))
    {
      WaitForSingleObject(proc_info.hProcess, INFINITE);
      GetExitCodeProcess(proc_info.hProcess, &exit_code);
      success = (exit_code == 0);

      CloseHandle(proc_info.hProcess);
      CloseHandle(proc_info.hThread);
    }
    else
      error("Process could not be launched : %s", buf);
  }
  else
    error("RC file '%s' incompletely written", out_files->rc.name);
  return success;
}

bool32
write_ir_file(OutFileNames* out_files, VmProgram* vm_program)
{
  int bytes_written = file_write_bytes(out_files->ir.name, (uint8*)vm_program->text.head, vm_program->text_len);
  bool32 success = (bytes_written == vm_program->text_len);
  if(!success)
    error("IR file '%s' incompletely written", out_files->ir.name);
  return success;
}

bool32
write_irc_file(OutFileNames* out_files, HasmCode* hasm_code)
{
  size_t bytes_written = file_write_bytes(out_files->irc.name, hasm_code->code_start, hasm_code->code_size);
  bool32 success = (bytes_written == hasm_code->code_size);
  if(!success)
    error("IRC file '%s' incompletely written", out_files->irc.name);
  return success;
}

int
main(int argc, char* argv[])
{
  bool32 success = true;

  if(success = (argc >= 2))
  {
    arena = arena_new(ARENA_SIZE);
    sym_arena = arena_push(arena, SYM_ARENA_SIZE);

    if(DEBUG_enabled)
    {/*>>>*/
      assert(DEBUG_ARENA_SIZE > 0);
      DEBUG_arena = arena_push(arena, DEBUG_ARENA_SIZE);
      DEBUG_print_sizeof_ast_structs();
    }/*<<<*/

    char* file_path = argv[1];
    char* hoc_text = file_read_text(arena, file_path);
    DEBUG_print_arena_usage("Read HoC text");

    if(success = to_bool(hoc_text))
    {
      VmProgram* vm_program = translate(file_path, hoc_text);
      if(success = vm_program->success)
      {
        if(DEBUG_enabled)
          printf("symbol count : %d\n", symtab->sym_count);
#if 0
        OutFileNames out_files = {0};
        char* file_stem = path_make_stem(file_path);

        bool32 success = make_file_names(&out_files, file_stem) &&
          write_ir_file(&out_files, vm_program);

        if(success)
        {
          HasmCode* hasm_code = 0;
          char* hasm_text = vm_program->text.head;
          bool32 success = translate_ir_to_code(&arena, hasm_text, &hasm_code);

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
  return success ? 0 : -1;
}