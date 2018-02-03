global_var int tempvar_id;

global_var Type* basic_type_bool;
global_var Type* basic_type_int;
global_var Type* basic_type_char;
global_var Type* basic_type_float;
global_var Type* basic_type_void;
global_var Type* basic_type_str;
global_var List* subst_list;
global_var int typevar_id = 1;

global_var int last_label_id;

global_var NextUse NextUse_None = max_int(); // infinity

void gen_label_name(MemoryArena* arena, Label* label)
{
  label->name = mem_push_array(arena, char, 12);
  platform_sprintf(label->name, "L_%d", last_label_id++);
}

char* new_tempvar_name(MemoryArena* arena, char* label)
{
  String str; str_init(arena, &str);
  str_printf(&str, "%s%d", label, tempvar_id++);
  return str_cap(&str);
}

void DEBUG_print_arena_usage(MemoryArena* arena, char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  platform_printf("in_use(`%s`) : %.2f%%\n", tag, usage.in_use*100);
}

#if 0/*>>>*/
void DEBUG_print_line(String* str, int indent_level, char* message, ...)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  va_list varargs;
  va_start(varargs, message);
  str_printf_va(str, message, varargs);
  va_end(varargs);

  str_append(str, "\n");
}

void DEBUG_print_type(String* str, int indent_level, char* tag, Type* type)
{
  for(int i = 0; i < indent_level; i++)
  {
    str_append(str, "  ");
  }

  if(tag)
  {
    str_append(str, tag);
    str_append(str, ": ");
  }

  make_type_printstr(str, type);
  str_append(str, "\n");
}

void DEBUG_print_ast_node(String* str, int indent_level, char* tag, AstNode* node)
{
  if(node)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }
#if 1
    if(node->src_loc)
    {
      DEBUG_print_line(str, indent_level, "%s src_line=\"%s:%d\"",
                       get_ast_printstr(node->kind), node->src_loc->file_path, node->src_loc->line_nr);
    }
    else
    {
      DEBUG_print_line(str, indent_level, "%s", get_ast_printstr(node->kind));
    }
#else
    if(node->src_loc)
    {
      DEBUG_print_line(str, indent_level, "src_line=\"%s:%d\"", node->src_loc->file_path, node->src_loc->line_nr);
    }
#endif
    ++indent_level;

    /* Example usage
       DEBUG_print_line(str, indent_level, "name: `%s`", node->proc.name);
       DEBUG_print_ast_node(str, indent_level, "id", node);
       DEBUG_print_ast_nodes(str, indent_level, "formal_args", &node->proc.formal_args);
    */
  }
}

void DEBUG_print_ast_nodes(String* str, int indent_level, char* tag, List* nodes)
{
  if(nodes->first)
  {
    if(tag)
    {
      DEBUG_print_line(str, indent_level, tag);
      ++indent_level;
    }

    for(ListItem* li = nodes->first;
        li;
        li = li->next)
    {
      AstNode* node = KIND(li, eList_ast_node)->ast_node;
      DEBUG_print_ast_node(str, indent_level, 0, node);
    }
  }
}
#endif/*<<<*/

void add_object_to_memory(X86Context* context, Symbol* object);

#include "lex.c"
#include "syntax.c"
#include "sym.c"
#include "type.c"
#include "ir_gen.c"
#include "x86_gen.c"

bool translate(MemoryArena* arena, char* title, char* file_path, char* hoc_text, String* x86_text)
{
  MemoryArena* gp_arena = push_arena(&arena, 2*MEGABYTE);

  basic_type_bool = new_basic_type(gp_arena, eBasicType_bool);
  basic_type_int = new_basic_type(gp_arena, eBasicType_int);
  basic_type_char = new_basic_type(gp_arena, eBasicType_char);
  basic_type_float = new_basic_type(gp_arena, eBasicType_float);
  basic_type_void = new_basic_type(gp_arena, eBasicType_void);
  basic_type_str = new_array_type(gp_arena, 0, 1, basic_type_char);
  subst_list = list_new(gp_arena, eList_type_pair);

  SymbolContext sym_context = {0};
  sym_context.gp_arena = gp_arena;
  sym_context.sym_arena = push_arena(&arena, 2*MEGABYTE);
  sym_context.nesting_depth = -1;
  sym_context.data_alignment = 4;
  list_init(sym_context.sym_arena, &sym_context.scopes, eList_scope);

  IrContext ir_context = {0};
  ir_context.gp_arena = gp_arena;
  ir_context.stmt_arena = push_arena(&arena, 2*MEGABYTE);
  ir_context.stmt_array = (IrStmt*)ir_context.stmt_arena->base;
  ir_context.stmt_count = 0;
  ir_context.sym_context = &sym_context;
  ir_context.label_list = list_new(ir_context.gp_arena, eList_ir_label);
  ir_context.data_alignment = 4;

  X86Context x86_context = {0};
  x86_context.gp_arena = gp_arena;
  x86_context.stmt_arena = push_arena(&arena, 2*MEGABYTE);
  x86_context.stmt_array = (X86Stmt*)x86_context.stmt_arena->base;
  x86_context.machine_word_size = 4;
  x86_init_registers(&x86_context);
  x86_context.text = x86_text;

  ir_context.x86_context = &x86_context;
  sym_context.x86_context = &x86_context;

  Parser* parser = parser_new(gp_arena);
  HFile* file = platform_open_file(gp_arena, file_path);
  parser_set_input(parser, hoc_text, file);

  if(!parse_module(parser))
  {
    return false;
  }

  AstNode* module = parser->module;

  if(!(sym_module(&sym_context, module) &&
       set_types_module(gp_arena, module) &&
       eval_types_module(gp_arena, module) &&
       resolve_types_module(gp_arena, module) &&
       check_types_module(gp_arena, module)))
  {
    return false;
  }

  ir_context.bool_true = new_const_object_int(&sym_context, 0, 1);
  ir_context.bool_false = new_const_object_int(&sym_context, 0, 0);
  x86_context.float_minus_one = new_const_object_float(&sym_context, 0, -1.0);

  if(!ir_gen_module(&ir_context, module))
  {
    return false;
  }

  alloc_scope_data_objects(&ir_context, module->module.scope);

  str_init(push_arena(&arena, 2*MEGABYTE), x86_context.text);
  x86_gen(&ir_context, &x86_context, module);

  return true;
}

