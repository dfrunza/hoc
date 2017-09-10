#include "hocc.h"

bool DEBUG_enabled = true;
bool DEBUG_zero_arena = true;
bool DEBUG_check_arena_bounds = true;

MemoryArena* arena = 0;
MemoryArena* symbol_table_arena = 0;

SymbolTable* symbol_table = 0;

Type* basic_type_bool;
Type* basic_type_int;
Type* basic_type_char;
Type* basic_type_float;
Type* basic_type_void;

#include "lib.cpp"
#include "lex.cpp"
#include "syntax.cpp"
#include "typecheck.cpp"
#include "semantic.cpp"
/*
#include "runtime.cpp"
#include "codegen.cpp"
#include "hasm.cpp"
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
  ArenaUsage sym_usage = arena_usage(symbol_table_arena);
  printf("-----  %s  -----\n", tag);
  printf("in_use(arena) : %.2f%%\n", usage.in_use*100);
  printf("in_use(symbol_table_arena) : %.2f%%\n", sym_usage.in_use*100);
}

void
DEBUG_print_sizeof_cst_structs()
{
  typedef struct 
  {
    CstKind kind;
    int size;
  }
  StructInfo;

#define make_struct_info(KIND)\
  {\
    CstNode node = {};\
    node.kind = CstKind_##KIND;\
    struct_info[CstKind_##KIND].kind = CstKind_##KIND;\
    struct_info[CstKind_##KIND].size = sizeof(node.##KIND);\
  }\

  StructInfo struct_info[CstKind__Count] = {};
  assert(CstKind__None == 0);
#if 1
  make_struct_info(bin_expr);
  make_struct_info(un_expr);
  make_struct_info(lit);
  make_struct_info(var_decl);
  make_struct_info(block);
  make_struct_info(proc);
  make_struct_info(id);
  make_struct_info(while_stmt);
  make_struct_info(for_stmt);
  make_struct_info(if_stmt);
  make_struct_info(ret_stmt);
  make_struct_info(goto_stmt);
  make_struct_info(label);
  make_struct_info(include);
  make_struct_info(module);
  make_struct_info(cast);
  make_struct_info(call);
  make_struct_info(array);
  make_struct_info(pointer);
  make_struct_info(struct_decl);
  make_struct_info(union_decl);
  make_struct_info(enum_decl);
  make_struct_info(init_list);
#endif

#undef make_struct_info
#undef make_zero_size_info

#if 1
  // insertion-sort the array
  for(int i = 1; i < CstKind__Count; i++)
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

  for(int i = CstKind__Count-1; i >= 0; i--)
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
  vm_program->instr_list = new_list(arena, ListKind_VmInstr);
  vm_program->success = false;

  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);

  CstNode* module_cst = 0;
  if(vm_program->success = parse(&token_stream, &module_cst))
  {
    assert(module_cst->kind == CstKind_module);
    if(DEBUG_enabled)/*>>>*/
    {
      DEBUG_print_arena_usage("Syntactic");

      begin_temp_memory(&arena);
      String* str = str_new(arena);
      DEBUG_print_cst_node(str, 0, module_cst, 0);
      str_dump_to_file(str, "debug_syntax.txt");
      end_temp_memory(&arena);
    }/*<<<*/

    AstNode* module_ast = 0;
    if(vm_program->success = build_ast(module_cst, &module_ast))
    {
      assert(symbol_table->scope_id == 0);
      assert(symbol_table->nesting_depth == 0);

#if 0
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Semantic");

        begin_temp_memory(&arena);
        String* str = str_new(arena);
        DEBUG_print_ast_node(str, 0, module_ast, 0);
        str_dump_to_file(str, "debug_semantic.txt");
        end_temp_memory(&arena);
      }/*<<<*/
#endif

#if 0
      build_runtime(module);
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Runtime");/*<<<*/
      }

      codegen(vm_program->instr_list, &vm_program->data, &vm_program->data_size, module);
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Codegen");/*<<<*/
      }

      str_init(&vm_program->text, arena);
      print_code(vm_program);
      if(DEBUG_enabled)/*>>>*/
      {
        DEBUG_print_arena_usage("Print code");/*<<<*/
      }
#endif
    }
  }
  return vm_program;
}

bool
make_out_file_names(OutFileNames* out_files, char* src_file_path)
{
  char* stem = mem_push_count_nz(arena, char, cstr_len(src_file_path));
  cstr_copy(stem, src_file_path);
  stem = path_make_stem(stem);

  int stem_len = cstr_len(stem);
  assert(stem_len > 0);
  bool success = true;

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

bool
write_hasm_file(OutFileNames* out_files, VmProgram* vm_program)
{
  int bytes_written = file_write_bytes(out_files->hasm.name, (uint8*)vm_program->text.head, vm_program->text_len);
  bool success = (bytes_written == vm_program->text_len);
  if(!success)
    error("HASM file '%s' incompletely written", out_files->hasm.name);
  return success;
}

int
main(int argc, char* argv[])
{
  bool success = true;

  if(success = (argc >= 2))
  {
    arena = new_arena(ARENA_SIZE);
    symbol_table_arena = push_arena(&arena, SYMBOL_TABLE_ARENA_SIZE);

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
          printf("symbol count : %d\n", symbol_table->sym_count);/*<<<*/
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
