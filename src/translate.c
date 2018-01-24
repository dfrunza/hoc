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

global_var Symbol* bool_true;
global_var Symbol* bool_false;

global_var NextUse NextUse_None = max_int(); // infinity

void gen_label_name(MemoryArena* arena, IrLabel* label)
{
  label->name = mem_push_array(arena, char, 12);
  h_sprintf(label->name, "L_%d", last_label_id++);
}

char* new_tempvar_name(char* label)
{
  String str; str_init(&str, arena);
  str_printf(&str, "%s%d", label, tempvar_id++);
  return str_cap(&str);
}

void DEBUG_print_arena_usage(MemoryArena* arena, char* tag)
{
  ArenaUsage usage = arena_usage(arena);
  h_printf("in_use(`%s`) : %.2f%%\n", tag, usage.in_use*100);
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

#include "lex.c"
#include "syntax.c"
#include "sym.c"
#include "type.c"
#include "ir_gen.c"
#include "x86_gen.c"

bool translate(char* title, char* file_path, char* hoc_text, String* x86_text)
{
  basic_type_bool = new_basic_type(eBasicType_bool);
  basic_type_int = new_basic_type(eBasicType_int);
  basic_type_char = new_basic_type(eBasicType_char);
  basic_type_float = new_basic_type(eBasicType_float);
  basic_type_void = new_basic_type(eBasicType_void);
  basic_type_str = new_array_type(0, 1, basic_type_char);
  subst_list = new_list(arena, eList_type_pair);
  
  SymbolContext sym_context = {0};
  sym_context.sym_arena = push_arena(&arena, 1*MEGABYTE);
  sym_context.nesting_depth = -1;
  init_list(&sym_context.scopes, sym_context.sym_arena, eList_scope);

  bool_true = new_const_object(sym_context.sym_arena, basic_type_int, 0);
  bool_true->int_val = 1;

  bool_false = new_const_object(sym_context.sym_arena, basic_type_int, 0);
  bool_false->int_val = 0;
  
  IrContext ir_context = {0};
  ir_context.stmt_arena = push_arena(&arena, 1*MEGABYTE);
  ir_context.stmt_array = (IrStmt*)ir_context.stmt_arena->base;
  ir_context.stmt_count = 0;
  ir_context.sym_arena = sym_context.sym_arena;
  ir_context.label_list = new_list(arena, eList_ir_label);
  ir_context.data_alignment = 4;
  
  TokenStream token_stream = {0};
  init_token_stream(&token_stream, hoc_text, file_path);
  get_next_token(&token_stream);
  
  AstNode* module = 0;
  if(!(parse_module(&token_stream, &module) &&
       sym_module(&sym_context, module) &&
       set_types_module(module) &&
       eval_types_module(module) &&
       resolve_types_module(module) &&
       check_types_module(module) &&
       ir_gen_module(&ir_context, module)))
  {
    return false;
  }

  X86Context x86_context = {0};
  x86_context.stmt_arena = push_arena(&arena, 1*MEGABYTE);
  x86_context.stmt_array = (X86Stmt*)x86_context.stmt_arena->base;
  x86_context.machine_word_size = 4;
  x86_init_registers(&x86_context);

  x86_gen(&ir_context, &x86_context, module, x86_text);

  return true;
}

