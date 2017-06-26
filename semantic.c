#include "lib.h"
#include "semantic.h"

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
add_symbol(MemoryArena* arena, SymbolTable* symtab, char* name, SymbolKind kind)
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
add_builtin_type(MemoryArena* arena, SymbolTable* symtab, char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  Symbol* symbol = add_symbol(arena, symtab, name, SymbolKind_Type);
  symbol->type = type;
  return symbol;
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

bool
semantic_analysis(MemoryArena* arena, AstNode* ast)
{
  init_types(arena);

  SymbolTable symtab = {0};
  symtab.arena = arena;

  add_builtin_type(arena, &symtab, "bool", basic_type_bool);
  add_builtin_type(arena, &symtab, "int", basic_type_int);
  add_builtin_type(arena, &symtab, "char", basic_type_char);
  add_builtin_type(arena, &symtab, "float", basic_type_float);
  add_builtin_type(arena, &symtab, "void", basic_type_void);

  bool success = true;
  if(ast->kind == AstNodeKind_Module)
  {

  }
  else
  {
    success = compile_error(&ast->src_loc, __FILE__, __LINE__, "ast->kind != AstNodeKind_Module");
  }

  return success;
}


