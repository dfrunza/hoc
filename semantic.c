////////////////////////////////////////////////////////////////////////////////
//  Symbol table
//

typedef struct
{
  Symbol* symbol;
  int scope_id;
  int last_scope_id;
  int nesting_depth;
  int active_scopes[32];
  MemoryArena* arena;
}
SymbolTable;

typedef enum
{
  SymbolKind__Null,
  SymbolKind_Keyword,
  SymbolKind_Proc,
  SymbolKind_Type,
  SymbolKind_Var,
}
SymbolKind;

typedef struct Symbol
{
  SymbolKind kind;
  Symbol* next_symbol;

  char* name;
  int block_id;
  int nesting_depth;

  union {
    TokenKind keyword;
    AstNode* node;
    Type* type;
  };
}
Symbol;

int
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

Symbol*
lookup_symbol(SymbolTable* symbol_table, char* name, SymbolKind kind)
{
  Symbol* result = 0;

  Symbol* symbol = symbol_table->symbol;
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

Symbol*
add_symbol(MemoryArena* arena, SymbolTable* symbol_table, char* name, SymbolKind kind)
{
  Symbol* symbol = mem_push_struct(arena, Symbol, 1);
  symbol->kind = kind;
  symbol->name = name;
  symbol->block_id = symbol_table->scope_id;
  symbol->nesting_depth = symbol_table->nesting_depth;
  symbol->next_symbol = symbol_table->symbol;
  symbol_table->symbol = symbol;
  return symbol;
}

Symbol*
add_builtin_type(MemoryArena* arena, SymbolTable* symbol_table, char* name, Type* type)
{
  assert(type->kind == TypeKind_Basic);
  Symbol* symbol = add_symbol(arena, symbol_table, name, SymbolKind_Type);
  symbol->type = type;
  return symbol;
}

Symbol*
add_keyword(MemoryArena* arena, SymbolTable* symbol_table, char* name, TokenKind token_kind)
{
  Symbol* symbol = add_symbol(arena, symbol_table, name, SymbolKind_Keyword);
  symbol->keyword = token_kind;
  return symbol;
}

void
add_keyword_list(MemoryArena* arena, SymbolTable* symbol_table)
{
  add_builtin_type(arena, symbol_table, "bool", basic_type_bool);
  add_builtin_type(arena, symbol_table, "int", basic_type_int);
  add_builtin_type(arena, symbol_table, "char", basic_type_char);
  add_builtin_type(arena, symbol_table, "float", basic_type_float);
  add_builtin_type(arena, symbol_table, "void", basic_type_void);

  add_keyword(arena, symbol_table, "var", TokenKind_Var);
  add_keyword(arena, symbol_table, "proc", TokenKind_Proc);
  add_keyword(arena, symbol_table, "if", TokenKind_If);
  add_keyword(arena, symbol_table, "else", TokenKind_Else);
  add_keyword(arena, symbol_table, "while", TokenKind_While);
  add_keyword(arena, symbol_table, "return", TokenKind_Return);
  add_keyword(arena, symbol_table, "break", TokenKind_Break);
  add_keyword(arena, symbol_table, "include", TokenKind_Include);
  add_keyword(arena, symbol_table, "true", TokenKind_True);
  add_keyword(arena, symbol_table, "false", TokenKind_False);
  add_keyword(arena, symbol_table, "print", TokenKind_Print);
  add_keyword(arena, symbol_table, "cast", TokenKind_Cast);
}

void
init_global_basic_types(MemoryArena* arena)
{
  basic_type_bool = new_basic_type(arena, BasicTypeKind_Bool);
  basic_type_int = new_basic_type(arena, BasicTypeKind_Int);
  basic_type_char = new_basic_type(arena, BasicTypeKind_Char);
  basic_type_float = new_basic_type(arena, BasicTypeKind_Float);
  basic_type_void = new_basic_type(arena, BasicTypeKind_Void);
}

bool32
scope_begin(SymbolTable* symbol_table)
{
  int scope_id = ++symbol_table->last_scope_id;
  symbol_table->scope_id = scope_id;

  int nesting_depth = ++symbol_table->nesting_depth;
  if(nesting_depth < sizeof_array(symbol_table->active_scopes))
  {
    symbol_table->active_scopes[nesting_depth] = scope_id;
  }
  else {
    error("Maximum scope nesting depth has been reached: %d", sizeof_array(symbol_table->active_scopes));
    return false;
  }

  return true;
}

void
scope_end(SymbolTable* symbol_table)
{
  int nesting_depth = --symbol_table->nesting_depth;
  int scope_id = symbol_table->active_scopes[nesting_depth];
  assert(scope_id >= 0);
  symbol_table->scope_id = scope_id;

  Symbol* symbol = symbol_table->symbol;
  while(symbol && symbol->block_id > symbol_table->scope_id)
    symbol = symbol->next_symbol;
  symbol_table->symbol = symbol;
}
