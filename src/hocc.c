#include "hocc.h"
#include "platform.h"

bool DEBUG_enabled = true;
bool DEBUG_zero_arena = true;
bool DEBUG_check_arena_bounds = true;

MemoryArena* arena = 0;

#include "lib.c"
#include "translate.c"

#include "platform.c"

typedef struct
{
  char* name;
  int len;
}
FileName;

typedef struct
{
  char strings[4*80 + 4*10];
  FileName h_asm;
  FileName source;
}
OutFileNames;

bool make_out_file_names(OutFileNames* out_files, char* src_file_path)
{
  char* leaf = mem_push_array_nz(arena, char, cstr_len(src_file_path));
  cstr_copy(leaf, src_file_path);
  leaf = path_make_leaf(leaf, false);

  int leaf_len = cstr_len(leaf);
  assert(leaf_len > 0);
  bool success = true;

  if(leaf_len <= 0 || leaf_len >= 81)
  {
    return success = error("length of file name must be between 1..80 : '%s'", leaf);
  }
  char* str = out_files->strings;

  sprintf(str, "%s.asm", leaf);
  out_files->h_asm.name = str;
  out_files->h_asm.len = cstr_len(out_files->h_asm.name);
  str = out_files->h_asm.name + out_files->h_asm.len + 1;

  sprintf(str, "%s", leaf);
  out_files->source.name = str;
  out_files->source.len = cstr_len(out_files->source.name);
  str = out_files->source.name + out_files->source.len + 1;

  return success;
}

int main(int argc, char* argv[])
{
  bool success = true;

  if(argc < 2)
  {
    success = error("missing argument : input source file");
    goto end;
  }
  char* src_file_path = argv[1];
  arena = new_arena(ARENA_SIZE);

  char* hoc_text = file_read_text(arena, src_file_path);
  if(DEBUG_enabled)/*>>>*/
  {
    printf("--- Read HoC text ---\n");
    DEBUG_print_arena_usage(arena, "arena");
  }/*<<<*/

  if(hoc_text == 0)
  {
    success = error("could not read source file `%s`", src_file_path);
    goto end;
  }
  OutFileNames out_files = {0};
  if(!make_out_file_names(&out_files, src_file_path))
  {
    success = false;
    goto end;
  }
  String x86_text = {0};
  if(!translate(out_files.source.name, src_file_path, hoc_text, &x86_text))
  {
    success = error("program could not be translated");
    goto end;
  }
  int x86_text_len = str_len(&x86_text);
  int bytes_written = file_write_bytes(out_files.h_asm.name, (uint8*)x86_text.head, x86_text_len);
  if(bytes_written != x86_text_len)
  {
    success = error("not all bytes were written to file `%s`", out_files.h_asm.name);
    goto end;
  }

#if 0
  OutFileNames out_files = {0};
  if(success = make_out_file_names(&out_files, src_file_path))
  {
    if(DEBUG_enabled)/*>>>*/
    {
      write_hasm_file(&out_files, ir_program);
    }/*<<<*/

    if(success = convert_hasm_to_instructions(ir_program))
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
          BinImage* bin_image = mem_push_struct(arena, BinImage);
          cstr_copy(bin_image->sig, BINIMAGE_SIGNATURE);

          bin_image->code_offset = sizeof(BinImage);
          bin_image->code_size = sizeof(Instruction) * ir_program->instr_count;

          bin_image->data_offset = bin_image->code_offset + bin_image->code_size;
          bin_image->data_size = sizeof(uint8) * ir_program->data_size;
          bin_image->sp = ir_program->sp;

          if((int)fwrite(vm_bytes, 1, vm_size, exe_file) == vm_size
            && (int)fwrite(bin_image, sizeof(BinImage), 1, exe_file) == 1
            && (int)fwrite(ir_program->instructions, sizeof(Instruction), ir_program->instr_count, exe_file) == ir_program->instr_count
            && (int)fwrite(ir_program->data, sizeof(uint8), ir_program->data_size, exe_file) == ir_program->data_size)
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
  }
#endif

#if 0
  getc(stdin);
#endif
end:
  return success ? 0 : -1;
}

