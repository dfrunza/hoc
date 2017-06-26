#include "hasm.c"
#include "syntax.h"
#include "semantic.h"

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

static bool debug_enabled = true;

VmProgram*
translate(MemoryArena* arena, char* file_path, char* hoc_text)
{
  VmProgram* vm_program = mem_push_struct(arena, VmProgram, 1);
  list_init(&vm_program->instr_list);
  vm_program->success = false;

  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(arena, &token_stream);

  AstNode* ast = 0;
  if(vm_program->success = parse(arena, &token_stream, &ast))
  {
    if(debug_enabled)
    {
      DEBUG_arena_print_occupancy("Parse", arena);

      String str = {0};
      str_init(&str, arena);
      DEBUG_print_ast_node(&str, 0, ast, 0);
      str_stdout(&str);
    }
#if 1
    vm_program->success = semantic_analysis(arena, ast);
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
    MemoryArena arena = arena_new(500*KILOBYTE);
    DEBUG_arena_print_occupancy("", &arena);

    char* file_path = argv[1];
    char* hoc_text = file_read_text(&arena, file_path);
    DEBUG_arena_print_occupancy("Read HoC text", &arena);

    if(hoc_text)
    {
      VmProgram* vm_program = translate(&arena, file_path, hoc_text);
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

  return ret;
}
