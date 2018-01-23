void translate()
{
  for(ListItem* li = symbol_table->scopes->first;
      li;
      li = li->next)
  {
    Scope* scope = KIND(li, eList_scope)->scope;
    int offset = 0;
    for(ListItem* li = scope->decls[eSymbol_var]->first;
        li;
        li = li->next)
    {
      Symbol* symbol = KIND(li, eList_symbol)->symbol;
      if(symbol->is_static_alloc)
      {
        symbol->data_loc = offset;
        offset += symbol->ty->width;
        scope->static_area_size += symbol->ty->width;
      }
      else
      {
        offset += symbol->ty->width;
        symbol->data_loc = -offset;
        scope->locals_area_size += symbol->ty->width;
      }
    }
    
    offset = 3*MACHINE_WORD_SIZE;
    for(ListItem* li = scope->decls[eSymbol_formal_arg]->first;
        li;
        li = li->next)
    {
      Symbol* symbol = KIND(li, eList_symbol)->symbol;
      symbol->data_loc = offset;
      offset += symbol->ty->width;
      scope->args_area_size += symbol->ty->width;
    }
    
    for(ListItem* li = scope->decls[eSymbol_ret_var]->first;
        li;
        li = li->next)
    {
      Symbol* symbol = KIND(li, eList_symbol)->symbol;
      symbol->data_loc = offset;
      offset += symbol->ty->width;
      scope->ret_area_size += symbol->ty->width;
    }
    
    //FIXME: This piece of code feels awfully out of place.
    for(ListItem* li = scope->decls[eSymbol_extern_proc]->first;
        li;
        li = li->next)
    {
      Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
      AstNode* proc = decl_sym->ast_node;
      assert(proc->kind == eAstNode_proc);
      assert(proc->proc.is_extern);
      
      int args_size = compute_type_width(decl_sym->ty->proc.args);
      char* label = proc->proc.label;
      String decorated_label; str_init(&decorated_label, arena);
      str_printf(&decorated_label, "%s@%d", label, args_size);
      proc->proc.label = str_cap(&decorated_label);
    }
  }
  
  str_init(x86_text, push_arena(&arena, X86_CODE_ARENA_SIZE));
  str_printfln(x86_text, ".686");
  str_printfln(x86_text, ".XMM");
  str_printfln(x86_text, ".MODEL flat, C");
  str_printfln(x86_text, ".STACK 4096");
  
  str_printfln(x86_text, ".DATA");
  str_printfln(x86_text, "static_area LABEL BYTE");
  Scope* module_scope = module->module.scope;
  for(ListItem* li = module_scope->decls[eSymbol_var]->first;
      li;
      li = li->next)
  {
    Symbol* symbol = KIND(li, eList_symbol)->symbol;
    int data_size = symbol->ty->width;
    if(symbol->data)
    {
      str_printf(x86_text, "BYTE ");
      uint8* p_data = (uint8*)symbol->data;
      int i;
      for(i = 0; i < data_size - 1; i++)
      {
        str_printf(x86_text, "0%xh,", p_data[i]);
      }
      if(i < data_size)
      {
        str_printf(x86_text, "0%xh", p_data[i]);
      }
      str_printfln(x86_text, "");
    }
    else
    {
      str_printfln(x86_text, "BYTE %d DUP(?) ; %s", data_size, symbol->name);
    }
  }
  
  str_printfln(x86_text, ".CODE");
  
  IrLabel label;
  str_printfln(x86_text, "_rt_module_prologue PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "push esp ;access link");
  str_printfln(x86_text, "sub esp, %d ;dummy IP", MACHINE_WORD_SIZE);
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "mov ebp, esp");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_module_prologue ENDP");
  
  str_printfln(x86_text, "_rt_block_prologue PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "add dword ptr [esp], %d ;access link", 2*MACHINE_WORD_SIZE);
  str_printfln(x86_text, "sub esp, %d ;dummy IP", MACHINE_WORD_SIZE);
  str_printfln(x86_text, "push ebp");
  str_printfln(x86_text, "mov ebp, esp");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_block_prologue ENDP");
  
  str_printfln(x86_text, "_rt_load PROC");
  str_printfln(x86_text, "pop ebx ;return address");
  str_printfln(x86_text, "pop ecx ;byte count");
  str_printfln(x86_text, "mov esi, dword ptr [esp]");
  str_printfln(x86_text, "mov edi, esp");
  str_printfln(x86_text, "rep movs byte ptr [edi], byte ptr [esi]");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_load ENDP");
  
  str_printfln(x86_text, "_rt_store PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  str_printfln(x86_text, "pop ecx");
  str_printfln(x86_text, "pop edi");
  str_printfln(x86_text, "mov esi, esp");
  str_printfln(x86_text, "rep movs byte ptr [edi], byte ptr [esi]");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_store ENDP");
  
  str_printfln(x86_text, "_rt_leave_frame PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  label = new_gen_label();
  str_printfln(x86_text, "pop ecx ;depth");
  str_printfln(x86_text, "%s$loop:", label.name);
  str_printfln(x86_text, "mov esp, ebp");
  str_printfln(x86_text, "pop ebp");
  str_printfln(x86_text, "loop %s$loop", label.name);
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_leave_frame ENDP");
  
  str_printfln(x86_text, "_rt_load_access_link PROC");
  str_printfln(x86_text, "pop ebx ;return address;");
  str_printfln(x86_text, "pop ecx ;declaration scope offset");
  str_printfln(x86_text, "mov esi, dword ptr [esp]");
  label = new_gen_label();
  str_printfln(x86_text, "%s$loop:", label.name);
  str_printfln(x86_text, "mov esi, dword ptr[esi]");
  str_printfln(x86_text, "loop %s$loop", label.name);
  str_printfln(x86_text, "mov dword ptr [esp], esi");
  str_printfln(x86_text, "push ebx");
  str_printfln(x86_text, "ret");
  str_printfln(x86_text, "_rt_load_access_link ENDP");
  
  if(!gen_x86(x86_text, module))
  {
    return false;
  }
  str_printfln(x86_text, "END");
}

void gen_x86_leave_frame(String* code, int depth)
{
  if(depth > 0)
  {
    str_printfln(code, "push %d", depth);
    str_printfln(code, "call _rt_leave_frame");
  }
  else if(depth == 0)
  {
    str_printfln(code, "mov esp, ebp");
    str_printfln(code, "pop ebp");
  }
  else
    assert(0);
}

void gen_x86_load_lvalue(String* code, AstNode* node)
{
  switch(node->kind)
  {
    case eAstNode_var_occur:
    {
      Symbol* occur_sym = node->var_occur.occur_sym;
      Symbol* decl_sym = occur_sym->decl;
      
      if(decl_sym->is_static_alloc)
      {
        assert(decl_sym->data_loc >=0 );
        str_printfln(code, "push OFFSET(static_area) + %d", decl_sym->data_loc);
      }
      else
      {
        str_printfln(code, "push ebp");
        
        int decl_scope_offset = occur_sym->scope->nesting_depth - decl_sym->scope->nesting_depth;
        if(decl_scope_offset > 0)
        {
          // Non-local
          str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
          str_printfln(code, "push %d", decl_scope_offset);
          str_printfln(code, "call _rt_load_access_link");
          
          // Load the FP by taking the offset relative to the Access Link
          str_printfln(code, "sub dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
        }
        else if(decl_scope_offset < 0)
          assert(0);
        
        if(decl_sym->data_loc >= 0)
        {
          str_printfln(code, "add dword ptr [esp], %d", decl_sym->data_loc);
        }
        else
        {
          str_printfln(code, "sub dword ptr [esp], %d", -decl_sym->data_loc);
        }
      }
    }
    break;
    
    case eAstNode_unr_expr:
    {
      AstNode* operand = node->unr_expr.operand;
      if(node->unr_expr.op == eOperator_deref)
      {
        assert(operand->ty->kind == eType_pointer);
        gen_x86_load_rvalue(code, operand);
      }
      else
        assert(0);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      AstNode* left_operand = node->bin_expr.left_operand;
      AstNode* right_operand = node->bin_expr.right_operand;
      
      if(node->bin_expr.op == eOperator_indexer)
      {
        gen_x86_load_lvalue(code, left_operand);
        gen_x86_load_rvalue(code, right_operand);
        
        str_printfln(code, "pop eax");
        str_printfln(code, "mov ebx, %d", node->eval_ty->width);
        str_printfln(code, "imul ebx");
        
        str_printfln(code, "add dword ptr [esp], eax");
      }
      else
        assert(0);
    }
    break;
    
    default:
    assert(0);
  }
}

void gen_x86_load_rvalue(String* code, AstNode* node)
{
  switch(node->kind)
  {
    case eAstNode_var_occur:
    {
      Type* type = ATTR(node, type, eval_type);
      gen_x86_load_lvalue(code, node);
      str_printfln(code, "push %d", type->width);
      str_printfln(code, "call _rt_load");
    }
    break;
    
    case eAstNode_call:
    gen_x86(code, node);
    break;
    
    case eAstNode_lit:
    {
      switch(node->lit.kind)
      {
        case eLiteral_int:
        str_printfln(code, "push %d", node->lit.int_val);
        break;
        case eLiteral_bool:
        str_printfln(code, "push %d", node->lit.bool_val);
        break;
        case eLiteral_float:
        {
          union BitcastF32ToI32
          {
            float32 float32_val;
            int32 int32_val;
          };
          
          union BitcastF32ToI32 val = {0};
          val.float32_val = node->lit.float_val;
          str_printfln(code, "push %xh ; %f", val.int32_val, val.float32_val);
        }
        break;
        case eLiteral_char:
        str_printfln(code, "push %d", node->lit.char_val);
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eAstNode_bin_expr:
    {
      if(node->bin_expr.op == eOperator_indexer)
      {
        gen_x86_load_lvalue(code, node);
        str_printfln(code, "push %d", node->eval_ty->width);
        str_printfln(code, "call _rt_load");
      }
      else
      {
        gen_x86(code, node);
      }
    }
    break;
    
    case eAstNode_unr_expr:
    {
      AstNode* operand = node->unr_expr.operand;
      
      if(node->unr_expr.op == eOperator_address_of)
      {
        if(operand->kind == eAstNode_var_occur)
        {
          gen_x86_load_lvalue(code, operand);
        }
        else
        {
          gen_x86_load_rvalue(code, operand);
        }
      }
      else if(node->unr_expr.op == eOperator_deref)
      {
        gen_x86_load_lvalue(code, node);
        str_printfln(code, "push %d", node->eval_ty->width);
        str_printfln(code, "call _rt_load");
      }
      else
      {
        gen_x86(code, node);
      }
    }
    break;
  }
}

bool gen_x86(String* code, AstNode* node)
{
  bool success = true;
  
  switch(node->kind)
  {
    case eAstNode_module:
    {
      Scope* module_scope = node->scope;
      for(ListItem* li = module_scope->decls[eSymbol_extern_proc]->first;
          li;
          li = li->next)
      {
        Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
        gen_x86(code, decl_sym->ast_node);
      }
      for(ListItem* li = module_scope->decls[eSymbol_proc].first;
          li;
          li = li->next)
      {
        Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
        gen_x86(code, decl_sym->ast_node);
      }
      
      AstNode* body = node->module.body;
      Scope* body_scope = body->block.scope;
      
      str_printfln(code, "startup PROC");
      str_printfln(code, "call _rt_module_prologue");
      str_printfln(code, "sub esp, %d ;alloc locals", body_scope->locals_area_size);
      for(ListItem* li = body->block.stmts.first;
          li;
          li = li->next)
      {
        AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
        gen_x86(code, stmt);
      }
      gen_x86_leave_frame(code, 0);
      str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE); // access link + dummy IP
      str_printfln(code, "ret");
      str_printfln(code, "startup ENDP");
    }
    break;
    
    case eAstNode_proc:
    {
      Scope* proc_scope = node->scope;
      
      char* label = node->proc.label;
      if(node->proc.is_extern)
      {
        str_printfln(code, "EXTERN %s:PROC", label);
      }
      else
      {
        for(ListItem* li = proc_scope->decls[eSymbol_extern_proc]->first;
            li;
            li = li->next)
        {
          Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
          gen_x86(code, decl_sym->ast_node);
        }
        for(ListItem* li = proc_scope->decls[eSymbol_proc].first;
            li;
            li = li->next)
        {
          Symbol* decl_sym = KIND(li, eList_symbol)->symbol;
          gen_x86(code, decl_sym->ast_node);
        }
        
        AstNode* body = node->proc.body;
        Scope* body_scope = body->block.scope;
        str_printfln(code, "%s PROC", node->proc.name);
        str_printfln(code, "push ebp");
        str_printfln(code, "mov ebp, esp");
        str_printfln(code, "sub esp, %d ;alloc locals", body_scope->locals_area_size);
        for(ListItem* li = body->block.stmts.first;
            li;
            li = li->next)
        {
          AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
          gen_x86(code, stmt);
        }
        gen_x86_leave_frame(code, 0);
        str_printfln(code, "ret");
        str_printfln(code, "%s ENDP", node->proc.name);
      }
    }
    break;
    
    case eAstNode_block:
    {
      Scope* scope = node->block.scope;
      
      str_printfln(code, "call _rt_block_prologue");
      str_printfln(code, "sub esp, %d", scope->locals_area_size);
      for(ListItem* li = node->block.stmts.first;
          li;
          li = li->next)
      {
        AstNode* stmt = KIND(li, eList_ast_node)->ast_node;
        gen_x86(code, stmt);
      }
      gen_x86_leave_frame(code, 0);
      str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE); // access link + dummy IP
    }
    break;
    
    case eAstNode_call:
    {
      AstNode* proc = node->call.proc;
      Scope* callee_scope = proc->scope;
      //        char* label = proc->proc.label;
      
      str_printfln(code, ";%s()", node->proc.name);
      str_printfln(code, "sub esp, %d ;alloc ret", callee_scope->ret_area_size);
      for(ListItem* li = node->call.actual_args.last;
          li;
          li = li->prev)
      {
        gen_x86_load_rvalue(code, KIND(li, eList_ast_node)->ast_node);
      }
      
      if(proc->proc.is_extern)
      {
        str_printfln(code, "call %s", node->proc.name);
        
        if(callee_scope->ret_area_size == MACHINE_WORD_SIZE)
        {
          str_printfln(code, "mov dword ptr [esp], eax ; save the return value");
        }
        else if(callee_scope->ret_area_size != 0)
          assert(0); //FIXME: don't know what to do
      }
      else
      {
        Symbol* occur_sym = node->occur_sym;
        Scope* caller_scope = occur_sym->scope;
        
        str_printfln(code, "push ebp");
        str_printfln(code, "add dword ptr [esp], %d", 2*MACHINE_WORD_SIZE);
        
        int callee_depth_offset = caller_scope->nesting_depth - callee_scope->nesting_depth;
        if(callee_depth_offset >= 0)
        {
          str_printfln(code, "push %d", callee_depth_offset + 1);
          str_printfln(code, "call _rt_load_access_link");
        }
        
        str_printfln(code, "call %s", node->proc.name);
        str_printfln(code, "add esp, %d ;dealloc args", MACHINE_WORD_SIZE + callee_scope->args_area_size);
      }
    }
    break;
    
    case eAstNode_stmt:
    {
      AstNode* actual_stmt = node->stmt.stmt;
      gen_x86(code, actual_stmt);
      str_printfln(code, "add esp, %d", actual_stmt->eval_ty->width);
    }
    break;
    
    case eAstNode_bin_expr:
    {
      AstNode* left_operand = node->bin_expr.left_operand;
      AstNode* right_operand = node->bin_expr.right_operand;
      
      switch(node->bin_expr.op)
      {
        case eOperator_assign:
        {
          gen_x86_load_rvalue(code, right_operand);
          gen_x86_load_lvalue(code, left_operand);
          str_printfln(code, "push %d", node->eval_ty->width);
          str_printfln(code, "call _rt_store");
        }
        break;
        
        case eOperator_add:
        {
          gen_x86_load_rvalue(code, left_operand);
          gen_x86_load_rvalue(code, right_operand);
          
          if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
          {
            str_printfln(code, "pop eax");
            str_printfln(code, "add dword ptr [esp], eax");
          }
          else if(types_are_equal(node->eval_ty, basic_type_float))
          {
            str_printfln(code, "movss xmm0, dword ptr [esp]");
            str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
            str_printfln(code, "addss xmm0, dword ptr [esp]");
            str_printfln(code, "movss dword ptr [esp], xmm0");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_sub:
        {
          if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
          {
            gen_x86_load_rvalue(code, left_operand);
            gen_x86_load_rvalue(code, right_operand);
            
            str_printfln(code, "pop eax");
            str_printfln(code, "sub dword ptr [esp], eax");
          }
          else if(types_are_equal(node->eval_ty, basic_type_float))
          {
            gen_x86_load_rvalue(code, right_operand);
            gen_x86_load_rvalue(code, left_operand);
            
            str_printfln(code, "movss xmm0, dword ptr [esp]");
            str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
            str_printfln(code, "subss xmm0, dword ptr [esp]");
            str_printfln(code, "movss dword ptr [esp], xmm0");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_mul:
        {
          gen_x86_load_rvalue(code, right_operand);
          gen_x86_load_rvalue(code, left_operand);
          
          if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
          {
            str_printfln(code, "pop eax");
            str_printfln(code, "imul dword ptr [esp]");
            str_printfln(code, "mov dword ptr [esp], eax");
          }
          else if(types_are_equal(node->eval_ty, basic_type_float))
          {
            str_printfln(code, "movss xmm0, dword ptr [esp]");
            str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
            str_printfln(code, "mulss xmm0, dword ptr [esp]");
            str_printfln(code, "movss dword ptr [esp], xmm0");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_div:
        {
          gen_x86_load_rvalue(code, right_operand);
          gen_x86_load_rvalue(code, left_operand);
          
          if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
          {
            str_printfln(code, "pop eax");
            str_printfln(code, "cdq"); // extend EAX into EDX
            str_printfln(code, "idiv dword ptr [esp]");
            str_printfln(code, "mov dword ptr [esp], eax");
          }
          else if(types_are_equal(node->eval_ty, basic_type_float))
          {
            str_printfln(code, "movss xmm0, dword ptr [esp]");
            str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
            str_printfln(code, "divss xmm0, dword ptr [esp]");
            str_printfln(code, "movss dword ptr [esp], xmm0");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_mod:
        {
          gen_x86_load_rvalue(code, right_operand);
          gen_x86_load_rvalue(code, left_operand);
          
          if(types_are_equal(node->eval_ty, basic_type_int) || node->eval_ty->kind == eType_pointer)
          {
            str_printfln(code, "pop eax");
            str_printfln(code, "cdq"); // extend EAX into EDX
            str_printfln(code, "idiv dword ptr [esp]");
            str_printfln(code, "mov dword ptr [esp], edx");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_eq:
        case eOperator_not_eq:
        {
          gen_x86_load_rvalue(code, left_operand);
          gen_x86_load_rvalue(code, right_operand);
          
          if(types_are_equal(left_operand->eval_ty, basic_type_char) || types_are_equal(left_operand->eval_ty, basic_type_int))
          {
            if(types_are_equal(left_operand->eval_ty, basic_type_int))
            {
              str_printfln(code, "pop eax");
              str_printfln(code, "pop ebx");
            }
            else if(types_are_equal(left_operand->eval_ty, basic_type_char))
            {
              str_printfln(code, "movzx eax, byte ptr [esp]");
              str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
              str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
            }
            
            str_printfln(code, "cmp ebx, eax");
          }
          else if(types_are_equal(left_operand->eval_ty, basic_type_float))
          {
            str_printfln(code, "movss xmm0, dword ptr [esp]");
            str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
            str_printfln(code, "comiss xmm0, dword ptr [esp]");
          }
          else
            assert(0);
          
          IrLabel label = new_gen_label();
          str_printfln(code, "push 1");
          if(node->bin_expr.op == eOperator_eq)
          {
            str_printfln(code, "jz %s$cmp_eq", label.name);
            str_printfln(code, "xor dword ptr [esp], 1");
            str_printfln(code, "%s$cmp_eq:", label.name);
          }
          else if(node->bin_expr.op == eOperator_not_eq)
          {
            str_printfln(code, "jnz %s$cmp_not_eq", label.name);
            str_printfln(code, "xor dword ptr [esp], 1");
            str_printfln(code, "%s$cmp_not_eq:", label.name);
          }
          else
            assert(0);
          //TODO: Do we need to explicitly handle the case when at least one of floating point operands is NaN,
          //or is there going to be some kind of CPU exception that will halt the program?
        }
        break;
        
        case eOperator_less:
        case eOperator_greater:
        case eOperator_less_eq:
        case eOperator_greater_eq:
        {
          if(types_are_equal(left_operand->eval_ty, basic_type_char) || types_are_equal(left_operand->eval_ty, basic_type_int))
          {
            gen_x86_load_rvalue(code, left_operand);
            gen_x86_load_rvalue(code, right_operand);
            
            if(types_are_equal(left_operand->eval_ty, basic_type_int))
            {
              str_printfln(code, "pop eax");
              str_printfln(code, "pop ebx");
            }
            else if(types_are_equal(left_operand->eval_ty, basic_type_char))
            {
              str_printfln(code, "movzx eax, byte ptr [esp]");
              str_printfln(code, "movzx ebx, byte ptr [esp+%d]", MACHINE_WORD_SIZE);
              str_printfln(code, "add esp, %d", 2*MACHINE_WORD_SIZE);
            }
            
            str_printfln(code, "cmp ebx, eax");
            
            IrLabel label = new_gen_label();
            str_printfln(code, "push 1");
            if(node->bin_expr.op == eOperator_less)
            {
              str_printfln(code, "jl %s$cmp_less", label.name);
              str_printfln(code, "xor dword ptr [esp], 1");
              str_printfln(code, "%s$cmp_less:", label.name);
            }
            else if(node->bin_expr.op == eOperator_less_eq)
            {
              str_printfln(code, "jle %s$cmp_less_eq", label.name);
              str_printfln(code, "xor dword ptr [esp], 1");
              str_printfln(code, "%s$cmp_less_eq:", label.name);
            }
            else if(node->bin_expr.op == eOperator_greater)
            {
              str_printfln(code, "jg %s$cmp_greater", label.name);
              str_printfln(code, "xor dword ptr [esp], 1");
              str_printfln(code, "%s$cmp_greater:", label.name);
            }
            else if(node->bin_expr.op == eOperator_greater_eq)
            {
              str_printfln(code, "jge %s$cmp_greater_eq", label.name);
              str_printfln(code, "xor dword ptr [esp], 1");
              str_printfln(code, "%s$cmp_greater_eq:", label.name);
            }
            else
              assert(0);
          }
          else if(types_are_equal(left_operand->eval_ty, basic_type_float))
          {
            if(node->bin_expr.op == eOperator_less)
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);
              str_printfln(code, "movss xmm0, dword ptr [esp]");
              str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
              str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
            }
            else if(node->bin_expr.op == eOperator_less_eq)
            {
              gen_x86_load_rvalue(code, right_operand);
              gen_x86_load_rvalue(code, left_operand);
              str_printfln(code, "movss xmm0, dword ptr [esp]");
              str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
              str_printfln(code, "cmpss xmm0, dword ptr [esp], 2");
            }
            else if(node->bin_expr.op == eOperator_greater)
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);
              str_printfln(code, "movss xmm0, dword ptr [esp]");
              str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
              str_printfln(code, "cmpss xmm0, dword ptr [esp], 1");
            }
            else if(node->bin_expr.op == eOperator_greater_eq)
            {
              gen_x86_load_rvalue(code, left_operand);
              gen_x86_load_rvalue(code, right_operand);
              str_printfln(code, "movss xmm0, dword ptr [esp]");
              str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
              str_printfln(code, "cmpss xmm0, dword ptr [esp], 2");
            }
            else
              assert(0);
            //TODO: Do we need to explicitly handle the case when at least one of floating point operands is NaN,
            //or is there going to be some kind of CPU exception that will halt the program?
            
            str_printfln(code, "movss dword ptr [esp], xmm0");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_logic_and:
        case eOperator_logic_or:
        {
          gen_x86_load_rvalue(code, left_operand);
          
          IrLabel label = new_gen_label();
          str_printfln(code, "pop eax");
          if(node->bin_expr.op == eOperator_logic_and)
          {
            str_printfln(code, "and eax, 1");
            str_printfln(code, "jz %s$logic_end", label.name);
          }
          else if(node->bin_expr.op == eOperator_logic_or)
          {
            str_printfln(code, "or eax, 0");
            str_printfln(code, "jnz %s$logic_end", label.name);
          }
          else
            assert(0);
          
          gen_x86_load_rvalue(code, right_operand);
          
          str_printfln(code, "pop eax");
          if(node->bin_expr.op == eOperator_logic_and)
          {
            str_printfln(code, "and eax, 1");
            str_printfln(code, "jz %s$logic_end", label.name);
          }
          else if(node->bin_expr.op == eOperator_logic_or)
          {
            str_printfln(code, "or eax, 0");
            str_printfln(code, "jnz %s$logic_end", label.name);
          }
          else
            assert(0);
          
          str_printfln(code, "%s$logic_end:", label.name);
          str_printfln(code, "push eax");
        }
        break;
        
        case eOperator_bit_and:
        case eOperator_bit_or:
        case eOperator_bit_xor:
        {
          gen_x86_load_rvalue(code, left_operand);
          gen_x86_load_rvalue(code, right_operand);
          
          str_printfln(code, "pop eax");
          if(node->bin_expr.op == eOperator_bit_and)
          {
            str_printfln(code, "and dword ptr [esp], eax");
          }
          else if(node->bin_expr.op == eOperator_bit_or)
          {
            str_printfln(code, "or dword ptr [esp], eax");
          }
          else if(node->bin_expr.op == eOperator_bit_xor)
          {
            str_printfln(code, "xor dword ptr [esp], eax");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_bit_shift_left:
        case eOperator_bit_shift_right:
        {
          gen_x86_load_rvalue(code, left_operand);
          gen_x86_load_rvalue(code, right_operand);
          
          str_printfln(code, "mov cl, byte ptr [esp]");
          str_printfln(code, "add esp, %d", MACHINE_WORD_SIZE);
          
          if(node->bin_expr.op == eOperator_bit_shift_left)
          {
            str_printfln(code, "shl dword ptr [esp], cl");
          }
          else if(node->bin_expr.op == eOperator_bit_shift_right)
          {
            str_printfln(code, "shr dword ptr [esp], cl");
          }
          else
            assert(0);
        }
        break;
        
        case eOperator_cast:
        {
          gen_x86_load_rvalue(code, right_operand);
          
          if(types_are_equal(left_operand->eval_ty, basic_type_int)
             && types_are_equal(right_operand->eval_ty, basic_type_float))
          {
            // int <- float
            str_printfln(code, "cvttss2si eax, dword ptr [esp]");
            str_printfln(code, "mov dword ptr [esp], eax");
          }
          if(types_are_equal(left_operand->eval_ty, basic_type_float)
             && types_are_equal(right_operand->eval_ty, basic_type_int))
          {
            // float <- int
            str_printfln(code, "cvtsi2ss xmm0, dword ptr [esp]");
            str_printfln(code, "movss dword ptr [esp], xmm0");
          }
        }
        break;
        
        case eOperator_indexer:
        {
          gen_x86_load_rvalue(code, node);
        }
        break;
        
        default: assert(0);
      }
    }
    break;
    
    case eAstNode_unr_expr:
    {
      AstNode* operand = node->unr_expr.operand;
      
      if(node->unr_expr.op == eOperator_address_of)
      {
        gen_x86_load_rvalue(code, node);
      }
      else if(node->unr_expr.op == eOperator_neg)
      {
        gen_x86_load_rvalue(code, operand);
        
        if(types_are_equal(operand->eval_ty, basic_type_int))
        {
          str_printfln(code, "neg dword ptr [esp]");
        }
        else if(types_are_equal(operand->eval_ty, basic_type_float))
        {
          str_printfln(code, "xor dword ptr [esp], %xh", 0x80000000);
        }
        else
          assert(0);
      }
      else if(node->unr_expr.op == eOperator_logic_not)
      {
        assert(0);
      }
      else if(node->unr_expr.op == eOperator_deref)
      {
        gen_x86_load_rvalue(code, node);
      }
      else
        assert(0);
    }
    break;
    
    case eAstNode_while:
    {
      IrLabel label = node->while_.label = new_gen_label();
      str_printfln(code, "%s$while_eval:", label.name);
      gen_x86_load_rvalue(code, node->while_.cond_expr);
      
      str_printfln(code, "pop eax");
      str_printfln(code, "and eax, 1");
      str_printfln(code, "jz %s$while_break", label.name);
      
      if(node->while_.body)
      {
        gen_x86(code, node->while_.body);
      }
      
      str_printfln(code, "jmp %s$while_eval", label.name);
      str_printfln(code, "%s$while_break:", label.name);
    }
    break;
    
    case eAstNode_if:
    {
      gen_x86_load_rvalue(code, node->if_.cond_expr);
      
      str_printfln(code, "pop eax");
      str_printfln(code, "and eax, 1");
      
      IrLabel label = new_gen_label();
      if(node->if_.else_body)
      {
        str_printfln(code, "jz %s$if_else", label.name);
      }
      else
      {
        str_printfln(code, "jz %s$if_end", label.name);
      }
      
      gen_x86(code, node->if_.body);
      str_printfln(code, "jmp %s$if_end", label.name);
      
      if(node->if_.else_body)
      {
        str_printfln(code, "%s$if_else:", label.name);
        gen_x86(code, node->if_.else_body);
      }
      
      str_printfln(code, "%s$if_end:", label.name);
    }
    break;
    
    case eAstNode_return:
    {
      if(node->ret.expr)
      {
        gen_x86(code, node->ret.expr);
      }
      gen_x86_leave_frame(code, node->ret.nesting_depth);
      str_printfln(code, "ret");
    }
    break;
    
    case eAstNode_loop_ctrl:
    {
      AstNode* loop = node->loop_ctrl.loop;
      gen_x86_leave_frame(code, node->loop_ctrl.nesting_depth);
      IrLabel* label = 0;
      if(loop->kind == eAstNode_while)
      {
        label = &loop->while_.label;
        if(node->loop_ctrl.kind == eLoopCtrl_break)
          str_printfln(code, "jmp %s$while_break", label->name);
        else if(node->loop_ctrl.kind == eLoopCtrl_continue)
          str_printfln(code, "jmp %s$while_eval", label->name);
        else
          assert(0);
      }
      else
        assert(0);
    }
    break;
    
    case eAstNode_empty:
    break; // skip
    
    case eAstNode_asm_block:
    str_append(code, node->asm_block.asm_text);
    break;
    
    default: assert(0);
  }
  return success;
}

