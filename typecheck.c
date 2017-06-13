Type*
new_basic_type(MemoryArena* arena, BasicTypeKind kind)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_Basic;
  type->basic.kind = kind;
  return type;
}

Type*
new_proc_type(MemoryArena* arena, Type* args, Type* ret)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_Proc;
  type->proc.args = args;
  type->proc.ret = ret;
  return type;
}

Type*
new_typevar(MemoryArena* arena)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_TypeVar;
  type->typevar.id = typevar_id++;
  return type;
}

Type*
new_product_type(MemoryArena* arena, Type* left, Type* right)
{
  Type* type = mem_push_struct(arena, Type, 1);
  type->kind = TypeKind_Product;
  type->product.left = left;
  type->product.right = right;
  return type;
}

bool32
types_are_equal(Type* type_a, Type* type_b)
{
  bool32 are_equal = false;

  if(type_a->kind != TypeKind_TypeVar &&
     type_b->kind == type_a->kind)
  {
    if(type_a->kind == TypeKind_Basic)
    {
      are_equal = (type_a->basic.kind == type_b->basic.kind);
    }
    else assert(false);
  }
  return are_equal;
}

Type*
copy_type(MemoryArena* arena, Type* type)
{
  Type* copy = mem_push_struct(arena, Type, 1);
  *copy = *type;
  return copy;
}

Type*
type_find_set_representative(Type* type)
{
  Type* result = type;
  while(type->repr_type)
  {
    type = type->repr_type;
    result = type;
  }
  return result;
}

void
type_set_union(Type* type_a, Type* type_b)
{
  if(type_a->kind == TypeKind_TypeVar)
  {
    type_a->repr_type = type_b;
  }
  else
  {
    type_b->repr_type = type_a;
  }
}

bool32
type_unification(Type* type_a, Type* type_b)
{
  bool32 success = false;
  Type* repr_type_a = type_find_set_representative(type_a);
  Type* repr_type_b = type_find_set_representative(type_b);

  if(repr_type_a->kind == TypeKind_TypeVar ||
     repr_type_b->kind == TypeKind_TypeVar)
  {
    type_set_union(repr_type_a, repr_type_b);
    success = true;
  }
  else if(repr_type_a->kind == repr_type_b->kind)
  {
    if(repr_type_a == repr_type_b)
    {
      success = true;
    }
    else if(repr_type_a->kind == TypeKind_Basic)
    {
      success = (repr_type_a->basic.kind == repr_type_b->basic.kind);
    }
    else
    {
      type_set_union(repr_type_a, repr_type_b);

      if(repr_type_a->kind == TypeKind_Proc)
      {
        success = type_unification(repr_type_a->proc.args, repr_type_b->proc.args) &&
          type_unification(repr_type_a->proc.ret, repr_type_b->proc.ret);
      }
      else if(repr_type_a->kind == TypeKind_Product)
      {
        success = type_unification(repr_type_a->product.left, repr_type_b->product.left) &&
          type_unification(repr_type_a->product.right, repr_type_b->product.right);
      }
      else if(repr_type_a->kind == TypeKind_Pointer)
      {
        success = type_unification(repr_type_a->ptr.pointee, repr_type_b->ptr.pointee);
      }
    }
  }

  return success;
}

Type*
make_product_type(MemoryArena* arena, Type* type_in, ListItem* list_item)
{
  Type* type_out = type_in;

  if(list_item)
  {
    AstNode* node = list_item->elem;
    Type* right_type = make_product_type(arena, node->type, list_item->next);
    type_out = new_product_type(arena, type_in, right_type);
  }
  return type_out;
}

TypeTuple*
new_type_tuple(MemoryArena* arena, Type* key, Type* value)
{
  TypeTuple* tuple = mem_push_struct(arena, TypeTuple, 1);
  tuple->key = key;
  tuple->value = value;
  return tuple;
}

TypeTuple*
type_find_tuple(List* tuple_list, Type* type)
{
  TypeTuple* result = 0;
  for(ListItem* list_item = list_first_item(tuple_list);
      list_item;
      list_item = list_item->next)
  {
    TypeTuple* tuple = list_item->elem;
    if(tuple->key == type)
    {
      result = tuple;
      break;
    }
  }
  return result;
}

Type*
type_substitution(MemoryArena* arena, List* tuple_list, Type* type)
{
  type = type_find_set_representative(type);
  Type* subst = 0;

  TypeTuple* tuple = type_find_tuple(tuple_list, type);
  if(tuple)
  {
    subst = tuple->value;
  }
  else
  {
    subst = copy_type(arena, type);

    tuple = new_type_tuple(arena, type, subst);
    list_append(arena, tuple_list, tuple);

    if(subst->kind == TypeKind_TypeVar)
    {
      subst->typevar.id = typevar_id++;
    }
    else if(subst->kind == TypeKind_Proc)
    {
      subst->proc.args = type_substitution(arena, tuple_list, subst->proc.args);
      subst->proc.ret = type_substitution(arena, tuple_list, subst->proc.ret);
    }
    else if(subst->kind == TypeKind_Product)
    {
      subst->product.left = type_substitution(arena, tuple_list, subst->product.left);
      subst->product.right = type_substitution(arena, tuple_list, subst->product.right);
    }
    else if(subst->kind == TypeKind_Pointer)
    {
      subst->ptr.pointee = type_substitution(arena, tuple_list, subst->ptr.pointee);
    }
    // else fall-thru
  }
  return subst;
}

bool32
typecheck_expr(MemoryArena* arena, List* type_tuples, AstNode* expr_node, Type** type)
{
  Type* result = 0;
  bool32 success = true;

  if(expr_node->kind == AstNodeKind_Literal)
  {
    result = type_substitution(arena, type_tuples, expr_node->type);
  }
  else if(expr_node->kind == AstNodeKind_VarOccur)
  {
    AstNode* var_decl_node = expr_node->var_occur.var_decl;
    if(success = type_unification(expr_node->type, var_decl_node->type))
    {
      result = type_substitution(arena, type_tuples, expr_node->type);
    }
    else
      compile_error(&expr_node->src_loc, "Type error: %s", expr_node->var_occur.name);
  }
  else if(expr_node->kind == AstNodeKind_BinExpr)
  {
    AstBinExpr* bin_expr = &expr_node->bin_expr;
    AstNode* left_operand = bin_expr->left_operand;
    AstNode* right_operand = bin_expr->right_operand;
    Type* left_type = 0;
    Type* right_type = 0;

    if(success = typecheck_expr(arena, type_tuples, left_operand, &left_type) &&
       typecheck_expr(arena, type_tuples, right_operand, &right_type))
    {
      if(success = type_unification(left_type, right_type))
      {
        if(is_logical_operator(bin_expr->op))
        {
          if(!(success = type_unification(expr_node->type, basic_type_bool)))
            compile_error(&expr_node->src_loc, "Type error: bool");
        }
        else
        {
          if(!(success = type_unification(expr_node->type, left_type)))
            compile_error(&expr_node->src_loc, "Type error: bin expr");
        }

        if(success)
          result = type_substitution(arena, type_tuples, expr_node->type);
      }
      else
        compile_error(&expr_node->src_loc, "Type error: bin expr");
    }
  }
  else if(expr_node->kind == AstNodeKind_UnrExpr)
  {
    AstUnrExpr* unr_expr = &expr_node->unr_expr;
    AstNode* operand = unr_expr->operand;
    Type* operand_type = 0;

    if(success = typecheck_expr(arena, type_tuples, operand, &operand_type))
    {
      if(unr_expr->op == AstOpKind_LogicNot)
      {
        if(!(success = type_unification(expr_node->type, basic_type_bool)))
          compile_error(&expr_node->src_loc, "Type error: bool");
      }
      else
      {
        if(!(success = type_unification(expr_node->type, operand_type)))
          compile_error(&expr_node->src_loc, "Type error: unr expr");
      }

      if(success)
      {
        result = type_substitution(arena, type_tuples, expr_node->type);
      }
    }
  }
  else if(expr_node->kind == AstNodeKind_Call)
  {
    AstCall* call = &expr_node->call;
    AstProc* proc = &call->proc->proc;
    assert(call->actual_args.count == proc->formal_args.count);

    // actual args
    {
      Type* arg_type = 0;
      for(ListItem* list_item = list_first_item(&call->actual_args);
          success && list_item;
          list_item = list_item->next)
      {
        AstNode* arg_node = list_item->elem;
        success = typecheck_expr(arena, type_tuples, arg_node, &arg_type) &&
          type_unification(arg_node->type, arg_type);
      }
    }

    // call type
    if(success)
    {
      Type* args_type = basic_type_void;
      int arg_count = call->actual_args.count;

      if(arg_count > 0)
      {
        ListItem* list_item = list_first_item(&call->actual_args);
        AstNode* arg_node = list_item->elem;
        args_type = make_product_type(arena, arg_node->type, list_item->next);
      }
      else if(arg_count < 0)
        assert(false);

      Type* proc_type = new_proc_type(arena, args_type, proc->ret_type);
      if(success = type_unification(proc_type, call->proc->type) &&
         type_unification(expr_node->type, proc->ret_type))
      {
        result = type_substitution(arena, type_tuples, expr_node->type);
      }
      else
        compile_error(&expr_node->src_loc, "Type error: %s(..)", call->name);
    }
  }
  else if(expr_node->kind == AstNodeKind_Cast)
  {
    Type* expr_type = 0;
    if(!typecheck_expr(arena, type_tuples, expr_node->cast.expr, &expr_type))
    {
      compile_error(&expr_node->src_loc, "Type error: cast expr");
      return false;
    }

    result = type_substitution(arena, type_tuples, expr_node->cast.to_type);

    {
      Type* type_from = type_find_set_representative(expr_type);
      Type* type_to = type_find_set_representative(result);
      if(!types_are_equal(type_from, type_to))
      {
        if(types_are_equal(type_from, basic_type_float) &&
           types_are_equal(type_to, basic_type_bool))
        {
          compile_error(&expr_node->src_loc, "Invalid cast : float -> bool");
          return false;
        }
        if(types_are_equal(type_from, basic_type_bool))
        {
          compile_error(&expr_node->src_loc, "Invalid cast : bool -> <something>");
          return false;
        }
      }
    }
  }
  else assert(false);

  *type = result;
  success = success && (result->kind != TypeKind_TypeVar);
//  if(!success)
//    compile_error(&expr_node->src_loc, "Type error: type var");
  return success;
}

bool32
typecheck_stmt(MemoryArena* arena, List* type_tuples, AstNode* stmt_node)
{
  bool32 success = true;

  if(stmt_node->kind == AstNodeKind_ReturnStmt)
  {
    AstReturnStmt* ret_stmt = &stmt_node->ret_stmt;
    Type* ret_type = basic_type_void;
    if(ret_stmt->ret_expr)
    {
      success = typecheck_expr(arena, type_tuples, ret_stmt->ret_expr, &ret_type) &&
        type_unification(ret_type, ret_stmt->proc->ret_type);
      if(!success)
        compile_error(&stmt_node->src_loc, "Type error: return stmt");
    }
    if(success)
    {
      success = type_unification(stmt_node->type, ret_type);
      if(!success)
        compile_error(&stmt_node->src_loc, "Type error: return stmt");
    }
  }
  else if(stmt_node->kind == AstNodeKind_VarDecl)
  {
    if(!(success = type_unification(stmt_node->type, stmt_node->var_decl.var_type)))
      compile_error(&stmt_node->src_loc, "Type error: %s", stmt_node->var_decl.name);
  }
  else if(stmt_node->kind == AstNodeKind_VarOccur)
  {
    AstNode* var_decl_node = stmt_node->var_occur.var_decl;
    if(!(success = type_unification(stmt_node->type, var_decl_node->var_decl.var_type)))
      compile_error(&stmt_node->src_loc, "Type error: %s", stmt_node->var_occur.name);
  }
  else if(stmt_node->kind == AstNodeKind_BinExpr)
  {
    Type* expr_type = 0;
    success = typecheck_expr(arena, type_tuples, stmt_node, &expr_type);
  }
  else if(stmt_node->kind == AstNodeKind_IfStmt)
  {
    AstIfStmt* if_stmt = &stmt_node->if_stmt;
    Type* cond_type = 0;
    if(success = typecheck_expr(arena, type_tuples, if_stmt->cond_expr, &cond_type))
    {
      if(success = type_unification(cond_type, basic_type_bool))
      {
        AstNode* body_node = if_stmt->body;
        if(body_node->kind == AstNodeKind_Block)
          success = typecheck_block(arena, type_tuples, body_node);
        else
          success = typecheck_stmt(arena, type_tuples, body_node);

        AstNode* else_node = if_stmt->else_body;
        if(else_node)
        {
          if(else_node->kind == AstNodeKind_Block)
            success = typecheck_block(arena, type_tuples, else_node);
          else
            success = typecheck_stmt(arena, type_tuples, else_node);
        }
      }
      else
        compile_error(&stmt_node->src_loc, "Type error: if stmt");
    }
  }
  else if(stmt_node->kind == AstNodeKind_WhileStmt)
  {
    AstWhileStmt* while_stmt = &stmt_node->while_stmt;
    Type* cond_type = 0;
    if(success = typecheck_expr(arena, type_tuples, while_stmt->cond_expr, &cond_type))
    {
      if(success = type_unification(cond_type, basic_type_bool))
      {
        AstNode* body_node = while_stmt->body;
        if(body_node->kind == AstNodeKind_Block)
          success = typecheck_block(arena, type_tuples, body_node);
        else
          success = typecheck_stmt(arena, type_tuples, body_node);
      }
      else
        compile_error(&stmt_node->src_loc, "Type error: while stmt");
    }
  }
  else if(stmt_node->kind == AstNodeKind_EmptyStmt)
  {
    // ok
  }
  else
    assert(false);

  Type* stmt_type = type_find_set_representative(stmt_node->type);
  success = success && (stmt_type->kind != TypeKind_TypeVar);
//  if(!success)
//    compile_error(&stmt_node->src_loc, "Type error: type var");
  return success;
}

bool32
typecheck_block(MemoryArena* arena, List* type_tuples, AstNode* block_node)
{
  //TODO: Typecheck the block properly:
  // If the block is owned by a proc, then its type should be set to proc's type.
  // If the block is owned by 'if,while' (or anonymous) then its type should be 'void'.
  bool32 success = true;
  assert(block_node->kind == AstNodeKind_Block);
  AstBlock* block = &block_node->block;

  // decl vars
  for(ListItem* list_item = list_first_item(&block->decl_vars);
      success && list_item;
      list_item = list_item->next)
  {
    AstNode* var_decl_node = list_item->elem;
    assert(var_decl_node->kind == AstNodeKind_VarDecl);
    AstVarDecl* var_decl = &var_decl_node->var_decl;

    success = type_unification(var_decl_node->type, var_decl->var_type);
  }
  if(success)
  {
    // statements
    for(ListItem* list_item = list_first_item(&block->stmt_list);
        success && list_item;
        list_item = list_item->next)
    {
      AstNode* stmt_node = list_item->elem;
      success = typecheck_stmt(arena, type_tuples, stmt_node);
    }
  }
  else
    error("Type error: block");

  return success;
}

bool32
typecheck_proc(MemoryArena* arena, List* type_tuples, AstNode* proc_node)
{
  bool32 success = true;
  assert(proc_node->kind == AstNodeKind_Proc);
  AstProc* proc = &proc_node->proc;

  // formal args
  for(ListItem* list_item = list_first_item(&proc->formal_args);
      success && list_item;
      list_item = list_item->next)
  {
    AstNode* var_decl_node = list_item->elem;
    assert(var_decl_node->kind == AstNodeKind_VarDecl);
    success = type_unification(var_decl_node->type, var_decl_node->var_decl.var_type);
  }

  if(success)
  {
    Type* args_type = basic_type_void;
    int arg_count = proc->formal_args.count;

    if(arg_count > 0)
    {
      ListItem* list_item = list_first_item(&proc->formal_args);
      AstNode* arg_node = list_item->elem;
      args_type = make_product_type(arena, arg_node->type, list_item->next);
    }
    else if(arg_count < 0)
      assert(false);

    Type* proc_type = new_proc_type(arena, args_type, proc->ret_type);
    if(success = type_unification(proc_type, proc_node->type))
    {
      success = typecheck_block(arena, type_tuples, proc->body);
    }
    else
      error("Type error: %s()", proc->name);
  }
  else
    error("Type error: %s()", proc->name);

  return success;
}

bool32
typecheck_module(MemoryArena* arena, List* type_tuples, AstNode* module_node)
{
  bool32 success = true;
  assert(module_node->kind == AstNodeKind_Module);
  AstModule* module = &module_node->module;

  for(ListItem* list_item = list_first_item(&module->proc_list);
      success && list_item;
      list_item = list_item->next)
  {
    AstNode* proc = list_item->elem;
    assert(proc->kind == AstNodeKind_Proc);
    success = typecheck_proc(arena, type_tuples, proc);
  }
  return success;
}
