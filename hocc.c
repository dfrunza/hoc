#include "hocc.h"
#include "lib.c"
#include "lex.c"
#include "syntax.c"
/*
#include "typecheck.c"
#include "semantic.c"
#include "runtime.c"
#include "codegen.c"
#include "hasm.c"
*/

typedef struct
{
  char* name;
  int len;
}
FileName;

typedef struct
{
  char strings[4*80 + 4*10];
  FileName hasm;
  FileName bincode;
  FileName exe;
}
OutFileNames;

void
DEBUG_print_arena_usage(char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  ArenaUsage sym_usage = arena_usage(sym_arena);
  printf("-----  %s  -----\n", tag);
  printf("in_use(arena) : %.2f%%\n", usage.in_use*100);
  printf("in_use(sym_arena) : %.2f%%\n", sym_usage.in_use*100);
}

void
DEBUG_print_sizeof_cst_structs()
{
  typedef struct 
  {
    CstNodeKind kind;
    int size;
  }
  StructInfo;

#define make_struct_info(KIND, STRUCT) \
  struct_info[KIND].kind = KIND; \
  struct_info[KIND].size = sizeof(STRUCT); \

  StructInfo struct_info[CstNodeKind__Count] = {0};
  assert(CstNodeKind__None == 0);
#if 1
  //make_struct_info(CstNodeKind__None, CstNode);
  make_struct_info(CstNodeKind_CstBinExpr, CstBinExpr);
  make_struct_info(CstNodeKind_CstUnaryExpr, CstUnaryExpr);
  make_struct_info(CstNodeKind_CstLiteral, CstLiteral);
  make_struct_info(CstNodeKind_CstVarDecl, CstVarDecl);
  make_struct_info(CstNodeKind_CstBlock, CstBlock);
  make_struct_info(CstNodeKind_CstProc, CstProc);
  make_struct_info(CstNodeKind_CstId, CstId);
  make_struct_info(CstNodeKind_CstWhileStmt, CstWhileStmt);
  make_struct_info(CstNodeKind_CstForStmt, CstForStmt);
  make_struct_info(CstNodeKind_CstIfStmt, CstIfStmt);
  make_struct_info(CstNodeKind_CstReturnStmt, CstReturnStmt);
  make_struct_info(CstNodeKind_CstGotoStmt, CstGotoStmt);
  make_struct_info(CstNodeKind_CstLabel, CstLabel);
  make_struct_info(CstNodeKind_CstInclude, CstInclude);
  make_struct_info(CstNodeKind_CstModule, CstModule);
  make_struct_info(CstNodeKind_CstCast, CstCast);
  make_struct_info(CstNodeKind_CstCall, CstCall);
  make_struct_info(CstNodeKind_CstArray, CstArray);
  make_struct_info(CstNodeKind_CstPointer, CstPointer);
  make_struct_info(CstNodeKind_CstStruct, CstStruct);
  make_struct_info(CstNodeKind_CstUnion, CstUnion);
  make_struct_info(CstNodeKind_CstEnum, CstEnum);
  make_struct_info(CstNodeKind_CstInitList, CstInitList);
#endif

#undef make_struct_info
#undef make_zero_size_info

#if 1
  // insertion-sort the array
  for(int i = 1; i < CstNodeKind__Count; i++)
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

  for(int i = CstNodeKind__Count-1; i >= 0; i--)
  {
    StructInfo* info = &struct_info[i];
    if(info->size > 0)
    {
      printf("%s.size = %d bytes\n", get_cst_kind_printstr(info->kind), info->size);
    }
  }
}

VmProgram*
translate(char* file_path, char* hoc_text)
{
  VmProgram* vm_program = mem_push_struct(arena, VmProgram);
  init_list(&vm_program->instr_list);
  vm_program->success = false;

  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  CstNode* module = 0;
  if(vm_program->success = parse(&token_stream, &module))
  {
    assert(module->kind == CstNodeKind_CstModule);
    if(DEBUG_enabled)/*>>>*/
    {
      DEBUG_print_arena_usage("Syntactic");

#if 0
      String* str = str_new(DEBUG_arena);
      DEBUG_print_cst_node(str, 0, module, 0);
      str_dump_to_file(str, "out_syntax.txt");
      free_arena(DEBUG_arena);
#else
      begin_temp_memory(&arena);
      String* str = str_new(arena);
      DEBUG_print_cst_node(str, 0, module, 0);
      str_dump_to_file(str, "debug_syntax.txt");
      end_temp_memory(&arena);
#endif
    }/*<<<*/

#if 0
    if(vm_program->success = semantic_analysis(module))
    {
      assert(symtab->block_id == 0);
      assert(symtab->nesting_depth == 0);
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Semantic");

        String* str = str_new(DEBUG_arena);
        DEBUG_print_cst_node(str, 0, (CstNode*)module, 0);
        str_dump_to_file(str, "out_semantic.txt");
        free_arena(DEBUG_arena);
      }/*<<<*/

      build_runtime(module);
      if(DEBUG_enabled)/*>>>*/
        DEBUG_print_arena_usage("Runtime");/*<<<*/

      codegen(&vm_program->instr_list, &vm_program->data, &vm_program->data_size, module);
      if(DEBUG_enabled)/*>>>*/
        DEBUG_print_arena_usage("Codegen");/*<<<*/

      str_init(&vm_program->text, arena);
      print_code(vm_program);
      if(DEBUG_enabled)/*>>>*/
        DEBUG_print_arena_usage("Print code");/*<<<*/
    }
#endif
  }
  return vm_program;
}

boole
make_out_file_names(OutFileNames* out_files, char* src_file_path)
{
  char* stem = mem_push_count_nz(arena, char, cstr_len(src_file_path));
  cstr_copy(stem, src_file_path);
  stem = path_make_stem(stem);

  int stem_len = cstr_len(stem);
  assert(stem_len > 0);
  boole success = true;

  if(success = (stem_len > 0 && stem_len < 81))
  {
    char* str = out_files->strings;

    sprintf(str, "%s.hasm", stem);
    out_files->hasm.name = str;
    out_files->hasm.len = cstr_len(out_files->hasm.name);
    str = out_files->hasm.name + out_files->hasm.len + 1;

    sprintf(str, "%s.bincode", stem);
    out_files->bincode.name = str;
    out_files->bincode.len = cstr_len(out_files->bincode.name);
    str = out_files->bincode.name + out_files->bincode.len + 1;

    sprintf(str, "%s.exe", stem);
    out_files->exe.name = str;
    out_files->exe.len = cstr_len(out_files->exe.name);
  }
  else
    error("length of file name must be between 1..80 : '%s'", stem);
  return success;
}

char*
make_vm_exe_path(char* hocc_exe_path)
{
  char* vm_exe_path = mem_push_count_nz(arena, char, cstr_len(hocc_exe_path) + cstr_len("vm.exe"));
  cstr_copy(vm_exe_path, hocc_exe_path);
  path_make_dir(vm_exe_path);
  cstr_append(vm_exe_path, "vm.exe");
  return vm_exe_path;
}

boole
write_hasm_file(OutFileNames* out_files, VmProgram* vm_program)
{
  int bytes_written = file_write_bytes(out_files->hasm.name, (uint8*)vm_program->text.head, vm_program->text_len);
  boole success = (bytes_written == vm_program->text_len);
  if(!success)
    error("HASM file '%s' incompletely written", out_files->hasm.name);
  return success;
}

int
main(int argc, char* argv[])
{
  boole success = true;

  if(success = (argc >= 2))
  {
    arena = new_arena(ARENA_SIZE);
    sym_arena = push_arena(&arena, SYM_ARENA_SIZE);

    if(DEBUG_enabled)/*>>>*/
    {
      begin_temp_memory(&arena);
      DEBUG_print_sizeof_cst_structs();
      end_temp_memory(&arena);
    }/*<<<*/

    char* src_file_path = argv[1];

    char* hoc_text = file_read_text(arena, src_file_path);
    if(DEBUG_enabled)/*>>>*/
      DEBUG_print_arena_usage("Read HoC text");/*<<<*/

    if(success = to_bool(hoc_text))
    {
      VmProgram* vm_program = translate(src_file_path, hoc_text);
      if(success = vm_program->success)
      {
#if 0
        if(DEBUG_enabled)/*>>>*/
          printf("symbol count : %d\n", symtab->sym_count);/*<<<*/
#endif
        
        OutFileNames out_files = {0};
        if(success = make_out_file_names(&out_files, src_file_path))
        {
#if 0
          BinCode* bin_image = mem_push_struct(arena, BinCode);
          cstr_copy(bin_image->sig, BINCODE_SIGNATURE);

          char* hasm_text = str_cap(&vm_program->text);

          if(DEBUG_enabled)/*>>>*/
            write_hasm_file(&out_files, vm_program);/*<<<*/

          if(success = convert_hasm_to_instructions(hasm_text, vm_program))
          {
            char* hocc_exe_path = argv[0];
            char* vm_exe_path = make_vm_exe_path(hocc_exe_path);

            uint8* vm_bytes = 0;
            int vm_size = 0;
            if(vm_size = file_read_bytes(arena, &vm_bytes, vm_exe_path))
            {
              FILE* exe_file = fopen(out_files.exe.name, "wb");
              if(exe_file)
              {
                bin_image->code_offset = sizeof(BinCode);
                bin_image->code_size = sizeof(Instruction) * vm_program->instr_count;

                bin_image->data_offset = bin_image->code_offset + bin_image->code_size;
                bin_image->data_size = sizeof(uint8) * vm_program->data_size;

                if((int)fwrite(vm_bytes, 1, vm_size, exe_file) == vm_size
                  && (int)fwrite(bin_image, sizeof(BinCode), 1, exe_file) == 1
                  && (int)fwrite(vm_program->instructions, sizeof(Instruction), vm_program->instr_count, exe_file) == vm_program->instr_count
                  && (int)fwrite(vm_program->data, sizeof(uint8), vm_program->data_size, exe_file) == vm_program->data_size)
                {
                  ;/*OK*/
                }
                else
                  success = error("could not write to file `%s`", out_files.exe.name);
                fclose(exe_file);
              }
              else
                success = error("could not write to file `%s`", out_files.exe.name);
            }
            else
              success = error("could not read file `%s`", vm_exe_path);
          }
#endif
        }
      }
      else
        success = error("program could not be translated");
    }
    else
      success = error("could not read source file `%s`", src_file_path);
  }
  else
    success = error("missing argument : input source file");

#if 0
  getc(stdin);
#endif
  return success ? 0 : -1;
}
