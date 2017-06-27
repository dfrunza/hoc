#include "hocc.h"

extern MemoryArena* arena;

extern Type* basic_type_bool;
extern Type* basic_type_int;
extern Type* basic_type_char;
extern Type* basic_type_float;
extern Type* basic_type_void;

#if 0
internal int
block_find_owner(AstBlock* block, AstNodeKind kind, AstNode** result)
{
  int depth = 0;
  AstNode* owner = 0;
  while(block)
  {
    owner = block->owner;
    if(owner->kind == kind)
      break;
    depth++;
    block = block->enclosing_block;
  }
  *result = owner;
  return depth;
}
#endif

internal Symbol*
lookup_symbol(SymbolTable* symtab, char* name, SymbolKind kind)
{
  Symbol* result = 0;

  Symbol* symbol = symtab->symbol;
  while(symbol)
  {
    if(symbol->kind == kind && cstr_match(symbol->name, name))
    {
      result = symbol;
      break;
    }
    symbol = symbol->next_symbol;
  }
  return result;
}

internal Symbol*
add_symbol(SymbolTable* symtab, char* name, SymbolKind kind)
{
  Symbol* symbol = mem_push_struct(arena, Symbol, 1);
  symbol->kind = kind;
  symbol->name = name;
  symbol->block_id = symtab->scope_id;
  symbol->nesting_depth = symtab->nesting_depth;
  symbol->next_symbol = symtab->symbol;
  symtab->symbol = symbol;
  return symbol;
}

Symbol*
add_builtin_type(SymbolTable* symtab, char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  Symbol* symbol = add_symbol(symtab, name, SymbolKind_Type);
  symbol->type = type;
  return symbol;
}

Symbol*
add_keyword(SymbolTable* symtab, char* name, TokenKind token_kind)
{
  Symbol* symbol = add_symbol(symtab, name, SymbolKind_Keyword);
  symbol->keyword = token_kind;
  return symbol;
}

void
register_builtin_symbols(SymbolTable* symtab)
{
  add_builtin_type(symtab, "bool", basic_type_bool);
  add_builtin_type(symtab, "int", basic_type_int);
  add_builtin_type(symtab, "char", basic_type_char);
  add_builtin_type(symtab, "float", basic_type_float);
  add_builtin_type(symtab, "void", basic_type_void);

  add_keyword(symtab, "var", TokenKind_Var);
  add_keyword(symtab, "proc", TokenKind_Proc);
  add_keyword(symtab, "if", TokenKind_If);
  add_keyword(symtab, "else", TokenKind_Else);
  add_keyword(symtab, "while", TokenKind_While);
  add_keyword(symtab, "for", TokenKind_For);
  add_keyword(symtab, "return", TokenKind_Return);
  add_keyword(symtab, "break", TokenKind_Break);
  add_keyword(symtab, "continue", TokenKind_Break);
  add_keyword(symtab, "include", TokenKind_Include);
  add_keyword(symtab, "true", TokenKind_True);
  add_keyword(symtab, "false", TokenKind_False);
}

internal bool
scope_begin(SymbolTable* symtab)
{
  int scope_id = ++symtab->last_scope_id;
  symtab->scope_id = scope_id;

  int nesting_depth = ++symtab->nesting_depth;
  if(nesting_depth < sizeof_array(symtab->active_scopes))
  {
    symtab->active_scopes[nesting_depth] = scope_id;
  }
  else {
    error("Maximum scope nesting depth has been reached: %d", sizeof_array(symtab->active_scopes));
    return false;
  }

  return true;
}

internal void
scope_end(SymbolTable* symtab)
{
  int nesting_depth = --symtab->nesting_depth;
  int scope_id = symtab->active_scopes[nesting_depth];
  assert(scope_id >= 0);
  symtab->scope_id = scope_id;

  Symbol* symbol = symtab->symbol;
  while(symbol && symbol->block_id > symtab->scope_id)
    symbol = symbol->next_symbol;
  symtab->symbol = symbol;
}

internal bool
module(SymbolTable* symtab, AstNode* ast)
{
  bool success = true;
  AstModule* ast_module = &ast->module;

  for(ListItem* list_item = ast_module->node_list.first;
      list_item;
      list_item = list_item->next)
  {

  }
  return success;
}

bool
semantic_analysis(AstNode* ast)
{
  init_types();

  SymbolTable symtab = {0};

  register_builtin_symbols(&symtab);

  bool success = true;
  assert(ast->kind == AstNodeKind_Module);
  success = module(&symtab, ast);

  return success;
}


