global_var int tempvar_id;
global_var int last_label_id;

global_var NextUse NextUse_None = max_int(); // infinity

bool is_operator_relation(eOperator op)
{
  bool is_relop = false;

  switch(op)
  {
    case eOperator::eq:
    case eOperator::not_eq_:
    case eOperator::less:
    case eOperator::less_eq:
    case eOperator::greater:
    case eOperator::greater_eq:
      is_relop = true;
    break;
  }

  return is_relop;
}

bool is_operator_logic(eOperator op)
{
  bool is_logic = false;

  switch(op)
  {
    case eOperator::logic_and:
    case eOperator::logic_or:
    case eOperator::logic_not:
      is_logic = true;
    break;
  }

  return is_logic;
}

void gen_label_name(MemoryArena* arena, Label* label)
{
  label->name = push_array(arena, char, 12);
  Platform::sprintf(label->name, "L_%d", last_label_id++);
}

char* gen_tempvar_name(MemoryArena* arena, char* label)
{
  String str = {};
  str.init(arena);
  str.printf("%s%d", label, tempvar_id++);
  return str.cap();
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
      AstNode* node = KIND(li, eList::ast_node)->ast_node;
      DEBUG_print_ast_node(str, indent_level, 0, node);
    }
  }
}
#endif/*<<<*/

#include "lex.cpp"
#include "syntax.cpp"
#include "sym.cpp"
#include "type.cpp"
#include "ir_gen.cpp"
#include "x86_gen.cpp"

bool translate(MemoryArena* arena, char* title, char* file_path, char* hoc_text, String** x86_text)
{
  MemoryArena* gp_arena = MemoryArena::push(&arena, 2*MEGABYTE);
  TypeContext* type_context = TypeContext::create(MemoryArena::push(&arena, 2*MEGABYTE));

  SymbolContext sym_context = {};
  sym_context.init(gp_arena, MemoryArena::push(&arena, 2*MEGABYTE), type_context);

  IrContext ir_context = {};
  ir_context.init(gp_arena, MemoryArena::push(&arena, 2*MEGABYTE), type_context, &sym_context);

  X86Context x86_context = {};
  x86_context.init(gp_arena, MemoryArena::push(&arena, 2*MEGABYTE), MemoryArena::push(&arena, 2*MEGABYTE),
                   type_context, &ir_context, &sym_context);

  Parser* parser = Parser::create(gp_arena);
  PlatformFile* file = Platform::file_open(gp_arena, file_path);
  parser->set_input(hoc_text, file);

  if(!parser->parse_module())
  {
    return false;
  }

  AstNode* module = parser->module;

  if(!(sym_context.process(module) && type_context->process(module)))
  {
    return false;
  }

  /* FIXME: Nasty dependencies */
  ir_context.bool_true = sym_context.create_const_int(0, 1);
  ir_context.bool_false = sym_context.create_const_int(0, 0);
  x86_context.float_minus_one = sym_context.create_const_float(0, -1.0);

  if(!ir_context.visit_module(module))
  {
    return false;
  }

  ir_context.partition_basic_blocks_module(module);
  ir_context.alloc_scope_data_objects(module->module.scope);

  x86_context.gen(module);
  *x86_text = x86_context.text;

  return true;
}

