void x86_print_register(String* text, X86Location* reg)
{
  switch(reg->kind)
  {
    /* 8-bit */
    case eX86Location_al:
      str_printf(text, "al");
    break;

    case eX86Location_ah:
      str_printf(text, "ah");
    break;

    case eX86Location_bl:
      str_printf(text, "bl");
    break;

    case eX86Location_bh:
      str_printf(text, "bh");
    break;

    case eX86Location_cl:
      str_printf(text, "cl");
    break;

    case eX86Location_ch:
      str_printf(text, "ch");
    break;

    case eX86Location_dl:
      str_printf(text, "dl");
    break;

    case eX86Location_dh:
      str_printf(text, "dh");
    break;

    /* 32-bit */
    case eX86Location_eax:
      str_printf(text, "eax");
    break;

    case eX86Location_ebx:
      str_printf(text, "ebx");
    break;

    case eX86Location_ecx:
      str_printf(text, "ecx");
    break;

    case eX86Location_edx:
      str_printf(text, "edx");
    break;

    case eX86Location_ebp:
      str_printf(text, "ebp");
    break;

    case eX86Location_esp:
      str_printf(text, "esp");
    break;

    case eX86Location_esi:
      str_printf(text, "esi");
    break;

    case eX86Location_edi:
      str_printf(text, "edi");
    break;

    /* xmm */
    case eX86Location_xmm0:
      str_printf(text, "xmm0");
    break;

    case eX86Location_xmm1:
      str_printf(text, "xmm1");
    break;

    case eX86Location_xmm2:
      str_printf(text, "xmm2");
    break;

    case eX86Location_xmm3:
      str_printf(text, "xmm3");
    break;

    case eX86Location_xmm4:
      str_printf(text, "xmm4");
    break;

    case eX86Location_xmm5:
      str_printf(text, "xmm5");
    break;

    case eX86Location_xmm6:
      str_printf(text, "xmm6");
    break;

    case eX86Location_xmm7:
      str_printf(text, "xmm7");
    break;

    default: assert(0);
  }
}

char* x86_make_type_directive(Type* type)
{
  char* directv = "";

  if(types_are_equal(type, basic_type_char))
  {
    directv = "byte";
  }
  else if(types_are_equal(type, basic_type_int) || types_are_equal(type, basic_type_float)
          || types_are_equal(type, basic_type_bool) || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    directv = "dword";
  }
  else assert(0);

  return directv;
}

void x86_print_operand(String* text, X86Operand* operand)
{
  switch(operand->kind)
  {
    case eX86Operand_memory:
    case eX86Operand_address:
    {
      if(operand->kind == eX86Operand_memory)
      {
        str_printf(text, "%s ptr [", x86_make_type_directive(operand->index.type));
      }
      else if(operand->kind == eX86Operand_address)
      {
        str_printf(text, "[");
      }
      else assert(0);

      X86Operand* base = operand->index.base;
      x86_print_operand(text, base);

      X86Operand* offset = operand->index.offset;
      if(offset)
      {
        char* sign = "+";
        if(offset->kind == eX86Operand_constant)
        {
          struct X86Operand_constant* constant = &offset->constant;
          assert(constant->kind == eX86Constant_int);

          if(constant->int_val < 0)
          {
            sign = "";
          }
        }
        str_printf(text, "%s", sign);
        x86_print_operand(text, offset);
      }
      str_printf(text, "]");
    }
    break;

    case eX86Operand_register:
    {
      x86_print_register(text, operand->reg);
    }
    break;

    case eX86Operand_constant:
    {
      struct X86Operand_constant* constant = &operand->constant;

      if(constant->kind == eX86Constant_int)
      {
        str_printf(text, "%d", constant->int_val);
      }
      else if(constant->kind == eX86Constant_float)
      {
        union BitcastFtoI
        {
          float float_val;
          int int_val;
        };

        union BitcastFtoI val = {0};
        val.float_val = constant->float_val;

        str_printf(text, "0%xh", val.int_val);
      }
      else if(constant->kind == eX86Constant_char)
      {
        str_printf(text, "'%c'", constant->char_val);
      }
      else assert(0);
    }
    break;

    case eX86Operand_id:
    {
      str_printf(text, "%s", operand->id);
    }
    break;

    default: assert(0);
  }
}

void x86_print_opcode(String* text, eX86Stmt opcode)
{
  switch(opcode)
  {
    case eX86Stmt_call:
      str_printf(text, "call ");
    break;

    case eX86Stmt_pop:
      str_printf(text, "pop ");
    break;

    case eX86Stmt_push:
      str_printf(text, "push ");
    break;

    case eX86Stmt_lea:
      str_printf(text, "lea ");
    break;

    /* integer ops */
    case eX86Stmt_mov:
      str_printf(text, "mov ");
    break;

    case eX86Stmt_add:
      str_printf(text, "add ");
    break;

    case eX86Stmt_sub:
      str_printf(text, "sub ");
    break;

    case eX86Stmt_imul:
      str_printf(text, "imul ");
    break;

    case eX86Stmt_cdq:
      str_printf(text, "cdq");
    break;

    case eX86Stmt_idiv:
      str_printf(text, "idiv ");
    break;

    case eX86Stmt_neg:
      str_printf(text, "neg ");
    break;

    case eX86Stmt_cmp:
      str_printf(text, "cmp ");
    break;

    /* integer jumps */
    case eX86Stmt_jz:
      str_printf(text, "jz ");
    break;

    case eX86Stmt_jnz:
      str_printf(text, "jnz ");
    break;

    case eX86Stmt_jl:
      str_printf(text, "jl ");
    break;

    case eX86Stmt_jle:
      str_printf(text, "jle ");
    break;

    case eX86Stmt_jg:
      str_printf(text, "jg ");
    break;

    case eX86Stmt_jge:
      str_printf(text, "jge ");
    break;

    /* floating point ops */
    case eX86Stmt_movss:
      str_printf(text, "movss ");
    break;

    case eX86Stmt_addss:
      str_printf(text, "addss ");
    break;

    case eX86Stmt_subss:
      str_printf(text, "subss ");
    break;

    case eX86Stmt_mulss:
      str_printf(text, "mulss ");
    break;

    case eX86Stmt_divss:
      str_printf(text, "divss ");
    break;

    case eX86Stmt_ucomiss:
      str_printf(text, "ucomiss ");
    break;

    /* floating point jumps */
    case eX86Stmt_jb:
      str_printf(text, "jb ");
    break;

    case eX86Stmt_jbe:
      str_printf(text, "jbe ");
    break;

    case eX86Stmt_ja:
      str_printf(text, "ja ");
    break;

    case eX86Stmt_jae:
      str_printf(text, "jae ");
    break;

    case eX86Stmt_je:
      str_printf(text, "je ");
    break;

    case eX86Stmt_jne:
      str_printf(text, "jne ");
    break;

    case eX86Stmt_jmp:
      str_printf(text, "jmp ");
    break;

    case eX86Stmt_nop:
      //str_printfln(text, "nop");
    break;

    case eX86Stmt_ret:
      str_printf(text, "ret ");
    break;

    default: assert(0);
  }
}

void x86_print_stmt(String* text, X86Stmt* stmt)
{
  switch(stmt->opcode)
  {
    case eX86Stmt_label:
    {
      x86_print_operand(text, stmt->operand1);
      str_printf(text, ":");
    }
    break;

    case eX86Stmt_extern_proc:
    {
      str_printf(text, "extern ");
      x86_print_operand(text, stmt->operand1);
      str_printf(text, ":proc");
    }
    break;

    default:
    {
      x86_print_opcode(text, stmt->opcode);
      if(stmt->operand1)
      {
        x86_print_operand(text, stmt->operand1);
      }

      if(stmt->operand2)
      {
        str_printf(text, ", ");
        x86_print_operand(text, stmt->operand2);
      }
    }
  }

  str_printfln(text, "");
}

X86Stmt* x86_new_stmt(X86Context* context, eX86Stmt opcode)
{
  X86Stmt* stmt = mem_push_struct(context->stmt_arena, X86Stmt);
  context->stmt_count++;
  stmt->opcode = opcode;
  stmt->operand1 = stmt->operand2 = 0;

  return stmt;
}

internal inline
bool is_object_in_location(Symbol* object, X86Location* loc)
{
  return (object->locations._[loc->kind] != 0);
}

void new_object_location_entry(X86Context* context, Symbol* object, X86Location* loc)
{
  object->locations._[loc->kind] = loc;

  List* occupants = &loc->occupants;
  for(ListItem* li = occupants->first;
      li;
      li = li->next)
  {
    assert(object != KIND(li, eList_symbol)->symbol);
  }
  append_list_elem(occupants, object, eList_symbol);
}

void delete_object_from_location(X86Context* context, Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    object->locations._[loc->kind] = 0;

    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;

    for(; li; li = li->next)
    {
      if(object == KIND(li, eList_symbol)->symbol)
        break;
    }
    if(li)
    {
      remove_list_item(occupants, li);
    }
  }
}

void add_object_to_location(X86Context* context, Symbol* object, X86Location* loc)
{
  if(is_object_in_location(object, loc))
  {
    List* occupants = &loc->occupants;
    ListItem* li = occupants->first;
    for(; li; li = li->next)
    {
      if(object == KIND(li, eList_symbol)->symbol)
        break;
    }
    assert(li);
  }
  else
  {
    new_object_location_entry(context, object, loc);
  }
}

void add_scope_objects_to_memory(X86Context* context, Scope* scope)
{
  List* objects = &scope->decl_syms;
  for(ListItem* li = objects->first;
      li;
      li = li->next)
  {
    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(!object->is_temp)
    {
      add_object_to_location(context, object, &context->memory);
    }
  }
}

bool reg_is_parent_free(X86Location* reg)
{
  bool is_free = true;

  List* occupants = &reg->occupants;

  if(occupants->count == 0)
  {
    if(reg->parent)
    {
      is_free = reg_is_parent_free(reg->parent);
    }
  }
  else
  {
    is_free = false;
  }

  return is_free;
}

bool reg_is_sub_free(X86Location* reg)
{
  bool is_free = true;

  List* occupants = &reg->occupants;

  if(occupants->count == 0)
  {
    if(reg->sub[0])
    {
      is_free = reg_is_sub_free(reg->sub[0]);
    }

    if(is_free && reg->sub[1])
    {
      is_free = reg_is_sub_free(reg->sub[1]);
    }
  }
  else
  {
    is_free = false;
  }

  return is_free;
}

bool reg_is_free(X86Location* reg)
{
  bool is_free = true;

  List* occupants = &reg->occupants;

  if(occupants->count == 0)
  {
    if(reg->parent)
    {
      is_free = reg_is_parent_free(reg->parent);
    }

    if(is_free && reg->sub[0])
    {
      is_free = reg_is_sub_free(reg->sub[0]);
    }

    if(is_free && reg->sub[1])
    {
      is_free = reg_is_sub_free(reg->sub[1]);
    }
  }
  else
  {
    is_free = false;
  }

  return is_free;
}

X86Location* reg_get_top(X86Location* reg)
{
  X86Location* top = reg;

  if(reg->parent)
  {
    top = reg_get_top(reg->parent);
  }

  return top;
}

void clean_register(X86Context* context, X86Location* loc)
{
  List* occupants = &loc->occupants;
  for(ListItem* li = occupants->first;
      li;
      li = li->next)
  {
    Symbol* occupant = KIND(li, eList_symbol)->symbol;
    delete_object_from_location(context, occupant, loc);
  }
}

void clean_register_all_levels(X86Context* context, X86Location* loc)
{
  clean_register(context, reg_get_top(loc));

  if(loc->sub[0])
  {
    clean_register(context, loc->sub[0]);
  }

  if(loc->sub[1])
  {
    clean_register(context, loc->sub[1]);
  }
}

void set_exclusive_object_location(X86Context* context, Symbol* object, X86Location* loc)
{
  for(int i = 0; i < context->register_count; i++)
  {
    delete_object_from_location(context, object, context->registers._[i]);
  }
  delete_object_from_location(context, object, &context->memory);

  add_object_to_location(context, object, loc);
}

internal inline
bool is_register_location(X86Context* context, X86Location* loc)
{
  bool is_register = false;

  for(int i = 0;
      i < context->register_count && !is_register;
      i++)
  {
    X86Location* reg = context->registers._[i];
    is_register = (reg == loc);
  }

  is_register |= (loc == &context->ebp) | (loc == &context->esp);

  return is_register;
}

internal inline
bool is_memory_location(X86Context* context, X86Location* loc)
{
  return (loc == &context->memory);
}

bool type_fits_into_register(Type* type, X86Location* reg)
{
  bool result = false;

  if(types_are_equal(type, basic_type_bool)
     || type->kind == eType_pointer || type->kind == eType_array)
  {
    result = types_are_equal(reg->type, basic_type_int);
  }
  else
  {
    result = types_are_equal(type, reg->type);
  }

  return result;
}

X86Location* find_free_register(X86Context* context, Type* type)
{
  X86Location* reg = 0;

  for(int i = 0; i < context->register_count; i++)
  {
    reg = context->registers._[i];
    if(reg_is_free(reg) && type_fits_into_register(type, reg))
    {
      break;
    }
    reg = 0;
  }

  return reg;
}

X86Location* lookup_object_location(X86Context* context, Symbol* object)
{
  X86Location* loc = 0;

  for(int i = 0; i < context->register_count; i++)
  {
    loc = context->registers._[i];
    if(is_object_in_location(object, loc))
      break;
    loc = 0;
  }

  if(!loc)
  {
    if(is_object_in_location(object, &context->memory) || object->kind == eSymbol_constant
       || !object->is_temp)
    {
      loc = &context->memory;
    }
  }

  return loc;
}

bool is_object_in_register(X86Context* context, Symbol* object)
{
  bool in_register = false;

  for(int i = 0;
      i < context->register_count && !in_register;
      i++)
  {
    X86Location* loc = object->locations._[i];
    in_register = (loc && is_register_location(context, loc));
  }

  return in_register;
}

bool is_single_occupant_register(X86Context* context, X86Location* reg, Symbol* object)
{
  assert(is_register_location(context, reg));

  bool result = false;
  List* occupants = &reg->occupants;
  if(occupants->count == 1)
  {
    Symbol* single_object = KIND(occupants->first, eList_symbol)->symbol;
    result = (single_object == object);
  }

  return result;
}

// X86 stack grows downwards
X86Operand* x86_make_index_operand(X86Context* context, eX86Operand kind, Symbol* object)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = kind;
  operand->index.type = object->ty;

  X86Operand* base = operand->index.base = mem_push_struct(arena, X86Operand);
  X86Operand* offset = operand->index.offset = mem_push_struct(arena, X86Operand);

  offset->kind = eX86Operand_constant;
  struct X86Operand_constant* constant = &offset->constant;
  constant->kind = eX86Constant_int;

  switch(object->storage_space)
  {
    case eStorageSpace_local:
    {
      base->kind = eX86Operand_register;
      base->reg = &context->ebp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace_static:
    {
      base->kind = eX86Operand_id;
      base->id = "static_area";

      constant->int_val = object->data_loc;
    }
    break;

    case eStorageSpace_param:
    {
      base->kind = eX86Operand_register;
      base->reg = &context->esp;

      constant->int_val = -(object->data_loc + object->allocd_size);
    }
    break;

    case eStorageSpace_arg:
    {
      base->kind = eX86Operand_register;
      base->reg = &context->ebp;

      int scope_allocd_size = object->scope->allocd_size;
      int machine_area_size = 2*context->machine_word_size; // instruction_pointer + frame_pointer
      constant->int_val = (scope_allocd_size + machine_area_size) - (object->data_loc + object->allocd_size);
    }
    break;

    default: assert(0);
  }

  return operand;
}

X86Operand* x86_make_register_operand(X86Context* context, X86Location* reg)
{
  assert(is_register_location(context, reg));

  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_register;
  operand->reg = reg;

  return operand;
}

X86Operand* x86_make_int_constant_operand(int int_val)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_constant;

  struct X86Operand_constant* constant = &operand->constant;
  constant->kind = eX86Constant_int;
  constant->int_val = int_val;

  return operand;
}

X86Operand* x86_make_memory_operand(Type* type, X86Operand* base, X86Operand* offset)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_memory;

  operand->index.type = type;
  operand->index.base = base;
  operand->index.offset = offset;

  return operand;
}

internal inline
X86Operand* x86_make_object_address_operand(X86Context* context, Symbol* object)
{
  return x86_make_index_operand(context, eX86Operand_address, object);
}

internal inline
X86Operand* x86_make_object_memory_operand(X86Context* context, Symbol* object)
{
  X86Operand* operand = 0;

  if(object->kind == eSymbol_constant)
  {
    if(types_are_equal(object->ty, basic_type_str))
    {
      operand = x86_make_object_address_operand(context, object);
    }
    else
    {
      operand = mem_push_struct(arena, X86Operand);
      operand->kind = eX86Operand_constant;

      struct X86Operand_constant* constant = &operand->constant;

      if(types_are_equal(object->ty, basic_type_int))
      {
        constant->kind = eX86Constant_int;
        constant->int_val = object->int_val;
      }
      else if(types_are_equal(object->ty, basic_type_float))
      {
        operand = x86_make_index_operand(context, eX86Operand_memory, object);
      }
      else if(types_are_equal(object->ty, basic_type_char))
      {
        constant->kind = eX86Constant_char;
        constant->char_val = object->char_val;
      }
      else if(types_are_equal(object->ty, basic_type_bool))
      {
        constant->kind = eX86Constant_int;
        constant->int_val = object->int_val;
      }
      else assert(0);
    }
  }
  else
  {
    operand = x86_make_index_operand(context, eX86Operand_memory, object);
  }

  return operand;
}

X86Operand* x86_make_object_operand(X86Context* context, Symbol* object)
{
  X86Operand* operand = 0;

  X86Location* object_loc = lookup_object_location(context, object);
  if(is_register_location(context, object_loc))
  {
    operand = x86_make_register_operand(context, object_loc);
  }
  else if(is_memory_location(context, object_loc))
  {
    operand = x86_make_object_memory_operand(context, object);
  }
  else assert(0);

  return operand;
}

X86Operand* x86_make_id_operand(char* id)
{
  X86Operand* operand = mem_push_struct(arena, X86Operand);
  operand->kind = eX86Operand_id;
  operand->id = id;
  return operand;
}

void x86_emit_mov(X86Context* context, Type* type, X86Operand* dest_operand, X86Operand* source_operand)
{
  eX86Stmt mov_op = eX86Stmt_None;

  if(types_are_equal(type, basic_type_int) || types_are_equal(type, basic_type_bool)
     || types_are_equal(type, basic_type_char)
     || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    mov_op = eX86Stmt_mov;
  }
  else if(types_are_equal(type, basic_type_float))
  {
    mov_op = eX86Stmt_movss;
  }

  X86Stmt* mov_stmt = x86_new_stmt(context, mov_op);
  mov_stmt->operand1 = dest_operand;
  mov_stmt->operand2 = source_operand;
}

void x86_load_object_value(X86Context* context, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(context, dest_loc));

  if(!is_object_in_location(object, dest_loc))
  {
    x86_emit_mov(context, object->ty,
                 x86_make_register_operand(context, dest_loc),
                 x86_make_object_operand(context, object));
  }
}

void x86_load_object_address(X86Context* context, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(context, dest_loc));

  if(!is_object_in_location(object, dest_loc))
  {
    X86Stmt* stmt = x86_new_stmt(context, eX86Stmt_lea);

    stmt->operand1 = x86_make_register_operand(context, dest_loc);
    stmt->operand2 = x86_make_object_address_operand(context, object);
  }
}

void x86_load_object(X86Context* context, Symbol* object, X86Location* dest_loc)
{
  assert(is_register_location(context, dest_loc));

  if(object->ty->kind == eType_array)
  {
    x86_load_object_address(context, object, dest_loc);
  }
  else
  {
    x86_load_object_value(context, object, dest_loc);
  }

  add_object_to_location(context, object, dest_loc);
}

void x86_store_object(X86Context* context, Symbol* object)
{
  if(!is_object_in_location(object, &context->memory) && (object->ty->kind != eType_array))
  {
    X86Location* object_loc = lookup_object_location(context, object);

    x86_emit_mov(context, object->ty,
                 x86_make_object_memory_operand(context, object),
                 x86_make_register_operand(context, object_loc));
  }
}

eX86Stmt conv_ir_op_to_x86_opcode(eIrOp ir_op, Type* type)
{
  eX86Stmt x86_opcode = eX86Stmt_None;

  if(types_are_equal(type, basic_type_int) || types_are_equal(type, basic_type_bool)
     || types_are_equal(type, basic_type_char)
     || (type->kind == eType_pointer) || (type->kind == eType_array))
  {
    switch(ir_op)
    {
      case eIrOp_add:
        x86_opcode = eX86Stmt_add;
      break;

      case eIrOp_sub:
        x86_opcode = eX86Stmt_sub;
      break;

      case eIrOp_mul:
        x86_opcode = eX86Stmt_imul;
      break;

      case eIrOp_div:
        x86_opcode = eX86Stmt_idiv;
      break;

      case eIrOp_less:
        x86_opcode = eX86Stmt_jl;
      break;

      case eIrOp_less_eq:
        x86_opcode = eX86Stmt_jle;
      break;

      case eIrOp_greater:
        x86_opcode = eX86Stmt_jg;
      break;

      case eIrOp_greater_eq:
        x86_opcode = eX86Stmt_jge;
      break;

      case eIrOp_eq:
        x86_opcode = eX86Stmt_jz;
      break;

      case eIrOp_not_eq:
        x86_opcode = eX86Stmt_jnz;
      break;

      case eIrOp_neg:
        x86_opcode = eX86Stmt_neg;
      break;

      default: assert(0);
    }
  }
  else if(types_are_equal(type, basic_type_float))
  {
    switch(ir_op)
    {
      case eIrOp_add:
        x86_opcode = eX86Stmt_addss;
      break;

      case eIrOp_sub:
        x86_opcode = eX86Stmt_subss;
      break;

      case eIrOp_mul:
        x86_opcode = eX86Stmt_mulss;
      break;

      case eIrOp_div:
        x86_opcode = eX86Stmt_divss;
      break;

      case eIrOp_less:
        x86_opcode = eX86Stmt_jb;
      break;

      case eIrOp_less_eq:
        x86_opcode = eX86Stmt_jbe;
      break;

      case eIrOp_greater:
        x86_opcode = eX86Stmt_ja;
      break;

      case eIrOp_greater_eq:
        x86_opcode = eX86Stmt_jae;
      break;

      case eIrOp_eq:
        x86_opcode = eX86Stmt_je;
      break;

      case eIrOp_not_eq:
        x86_opcode = eX86Stmt_jne;
      break;

      default: assert(0);
    }
  }
  else assert(0);


  return x86_opcode;
}

X86Location* find_least_used_register(X86Context* context, Type* type)
{
  X86Location* result = 0;

  int next_use = max_int();

  for(int i = 0; i < context->register_count; i++)
  {
    X86Location* reg = context->registers._[i];

    if(type_fits_into_register(type, reg))
    {
      List* occupants = &reg->occupants;

      for(ListItem* li = occupants->first;
          li;
          li = li->next)
      {
        Symbol* object = KIND(li, eList_symbol)->symbol;

        if(!object->is_live || object->next_use >= next_use)
        {
          next_use = object->next_use;
          result = reg;
        }
      }
    }
  }
  assert(result);

  return result;
}

void save_object_to_memory(X86Context* context, Symbol* object)
{
  x86_store_object(context, object);
  add_object_to_location(context, object, &context->memory);
}

void save_register(X86Context* context, X86Location* reg, bool free_reg)
{
  List* occupants = &reg->occupants;

  for(ListItem* li = occupants->first; li; )
  {
    ListItem* li_next = li->next;

    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(object->is_live)
    {
      save_object_to_memory(context, object);
    }

    if(free_reg)
    {
      delete_object_from_location(context, object, reg);
    }

    li = li_next;
  }
}

void save_register_all_levels(X86Context* context, X86Location* loc, bool free_reg)
{
  save_register(context, reg_get_top(loc), free_reg);

  if(loc->sub[0])
  {
    save_register(context, loc->sub[0], free_reg);
  }

  if(loc->sub[1])
  {
    save_register(context, loc->sub[1], free_reg);
  }
}

X86Location* get_best_available_register(X86Context* context, Type* type)
{
  X86Location* best_reg = 0;

  X86Location* free_reg = find_free_register(context, type);
  if(free_reg)
  {
    best_reg = free_reg;
  }
  else
  {
    if(best_reg)
    {
      save_register(context, best_reg, true);
    }
    else
    {
      best_reg = find_least_used_register(context, type);
      save_register(context, best_reg, true);
    }
  }

  return best_reg;
}

void discard_unused_arg(X86Context* context, IrArg* arg, X86Location* arg_loc)
{
  if(is_register_location(context, arg_loc)
     && (arg->next_use == NextUse_None && !arg->is_live))
  {
    delete_object_from_location(context, arg->object, arg_loc);
  }
}

void discard_all_unused_args(X86Context* context, struct IrStmt_assign* assign)
{
  X86Location* arg1_loc = lookup_object_location(context, assign->arg1->object);
  discard_unused_arg(context, assign->arg1, arg1_loc);

  if(assign->arg2)
  {
    X86Location* arg2_loc = lookup_object_location(context, assign->arg2->object);
    discard_unused_arg(context, assign->arg2, arg2_loc);
  }
}

void save_all_registers(X86Context* context, bool free_reg)
{
  for(int i = 0; i < context->register_count; i++)
  {
    X86Location* reg = context->registers._[i];
    save_register(context, reg, free_reg);
  }
}

void x86_gen_divmod_op(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  assert(assign->op == eIrOp_div || assign->op == eIrOp_mod);

  if(types_are_equal(arg1->object->ty, basic_type_int))
  {
    X86Location* remainder_loc = &context->edx;
    save_register_all_levels(context, remainder_loc, true);

    X86Location* dividend_loc = &context->eax;
    save_register_all_levels(context, dividend_loc, true);
    if(!is_object_in_location(arg1->object, dividend_loc))
    {
      save_object_to_memory(context, arg1->object);
      x86_load_object(context, arg1->object, dividend_loc);
    }

    X86Stmt* x86_stmt = x86_new_stmt(context, eX86Stmt_cdq);

    X86Location* divisor_loc = lookup_object_location(context, arg2->object);
    if(!is_register_location(context, divisor_loc))
    {
      divisor_loc = get_best_available_register(context, arg2->object->ty);
      x86_load_object(context, arg2->object, divisor_loc);
    }

    x86_stmt = x86_new_stmt(context, eX86Stmt_idiv);
    x86_stmt->operand1 = x86_make_object_operand(context, arg2->object);

    if(assign->op == eIrOp_div)
    {
      clean_register_all_levels(context, dividend_loc);
      set_exclusive_object_location(context, result->object, dividend_loc);
    }
    else if(assign->op == eIrOp_mod)
    {
      clean_register_all_levels(context, remainder_loc);
      set_exclusive_object_location(context, result->object, remainder_loc);
    }
    else assert(0);
  }
  else if(types_are_equal(arg1->object->ty, basic_type_char))
  {
    fail("TODO");
  }
  else assert(0);
}

// result = arg1[arg2]
void x86_gen_index_source(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = get_best_available_register(context, result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(context, arg2->object);
  if(!is_register_location(context, arg2_loc))
  {
    arg2_loc = get_best_available_register(context, arg2->object->ty);
    x86_load_object(context, arg2->object, arg2_loc);
  }

  x86_emit_mov(context, result->object->ty,
               x86_make_register_operand(context, result_loc),
               x86_make_memory_operand(result->object->ty,
                                       x86_make_register_operand(context, arg1_loc),
                                       x86_make_register_operand(context, arg2_loc)));

  clean_register_all_levels(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

// result[arg2] = arg1
void x86_gen_index_dest(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = get_best_available_register(context, result->object->ty);
    x86_load_object(context, result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = get_best_available_register(context, arg1->object->ty);
    assert(arg1_loc != result_loc);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  X86Location* arg2_loc = lookup_object_location(context, arg2->object);
  if(!is_register_location(context, arg2_loc))
  {
    arg2_loc = get_best_available_register(context, arg2->object->ty);
    assert(arg2_loc != result_loc && arg2_loc != arg1_loc);
    x86_load_object(context, arg2->object, arg2_loc);
  }

  x86_emit_mov(context, arg1->object->ty,
               x86_make_memory_operand(arg1->object->ty,
                                       x86_make_register_operand(context, result_loc),
                                       x86_make_register_operand(context, arg2_loc)),
               x86_make_register_operand(context, arg1_loc));
}

// result = ^arg1
void x86_gen_deref_source(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = get_best_available_register(context, result->object->ty);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  x86_emit_mov(context, result->object->ty,
               x86_make_register_operand(context, result_loc),
               x86_make_memory_operand(result->object->ty, x86_make_register_operand(context, arg1_loc), 0));

  clean_register_all_levels(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

// ^result = arg1
void x86_gen_deref_dest(X86Context* context, struct IrStmt_assign *assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = lookup_object_location(context, result->object);
  if(!is_register_location(context, result_loc))
  {
    result_loc = get_best_available_register(context, result->object->ty);
    x86_load_object(context, result->object, result_loc);
  }

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  if(!is_register_location(context, arg1_loc))
  {
    arg1_loc = get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  x86_emit_mov(context, arg1->object->ty,
               x86_make_memory_operand(arg1->object->ty, x86_make_register_operand(context, result_loc), 0),
               x86_make_register_operand(context, arg1_loc));
}

// result = arg1
void x86_gen_equal(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);

  if(is_register_location(context, arg1_loc))
  {
    set_exclusive_object_location(context, result->object, arg1_loc);
  }
  else if(is_memory_location(context, arg1_loc))
  {
    X86Location* result_loc = lookup_object_location(context, result->object);

    if(result->next_use == NextUse_None
       && is_register_location(context, result_loc)
       && is_single_occupant_register(context, result_loc, result->object))
    {
      x86_load_object(context, arg1->object, result_loc);
    }
    else
    {
      assert(is_memory_location(context, result_loc));

      result_loc = get_best_available_register(context, result->object->ty);
      x86_load_object(context, arg1->object, result_loc);
    }

    clean_register_all_levels(context, result_loc);
    set_exclusive_object_location(context, result->object, result_loc);
  }
  else assert(0);
}

// result = &arg1
void x86_gen_address_of(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* result_loc = get_best_available_register(context, result->object->ty);
  x86_load_object_address(context, arg1->object, result_loc);

  clean_register_all_levels(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

// result = arg1 op arg2
void x86_gen_bin_expr(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  if(assign->op == eIrOp_mod)
  {
    x86_gen_divmod_op(context, assign);
  }
  else if(assign->op == eIrOp_div)
  {
    x86_gen_divmod_op(context, assign);
  }
  else
  {
    X86Location* arg1_loc = lookup_object_location(context, arg1->object);
    X86Location* result_loc = arg1_loc;

    if(is_register_location(context, arg1_loc)
       && is_single_occupant_register(context, arg1_loc, arg1->object)
       && (arg1->next_use == NextUse_None && !arg1->is_live))
    {
      delete_object_from_location(context, arg1->object, arg1_loc);
    }
    else
    {
      result_loc = get_best_available_register(context, arg1->object->ty);
      x86_load_object(context, arg1->object, result_loc);
    }

    X86Stmt* x86_stmt = x86_new_stmt(context, conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
    x86_stmt->operand1 = x86_make_register_operand(context, result_loc);
    x86_stmt->operand2 = x86_make_object_operand(context, arg2->object);

    clean_register_all_levels(context, result_loc);
    set_exclusive_object_location(context, result->object, result_loc);
  }
}

// result = op arg1
void x86_gen_unr_expr(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;

  X86Location* arg1_loc = lookup_object_location(context, arg1->object);
  X86Location* result_loc = arg1_loc;

  if(is_register_location(context, arg1_loc)
      && is_single_occupant_register(context, arg1_loc, arg1->object)
      && (arg1->next_use == NextUse_None && !arg1->is_live))
  {
    delete_object_from_location(context, arg1->object, arg1_loc);
  }
  else
  {
    result_loc = get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, result_loc);
  }

  if(assign->op == eIrOp_neg && types_are_equal(arg1->object->ty, basic_type_float))
  {
    X86Stmt* x86_stmt = x86_new_stmt(context, eX86Stmt_mulss);
    x86_stmt->operand1 = x86_make_register_operand(context, result_loc);
    x86_stmt->operand2 = x86_make_object_operand(context, context->float_minus_one);
  }
  else
  {
    X86Stmt* x86_stmt = x86_new_stmt(context, conv_ir_op_to_x86_opcode(assign->op, result->object->ty));
    x86_stmt->operand1 = x86_make_register_operand(context, result_loc);
  }

  clean_register_all_levels(context, result_loc);
  set_exclusive_object_location(context, result->object, result_loc);
}

void x86_gen_assign(X86Context* context, struct IrStmt_assign* assign)
{
  IrArg* result = assign->result;
  IrArg* arg1 = assign->arg1;
  IrArg* arg2 = assign->arg2;

  if(assign->op)
  {
    if(arg2)
    {
      if(assign->op == eIrOp_index_source)
      {
        // result = arg1[arg2]
        x86_gen_index_source(context, assign);
      }
      else if(assign->op == eIrOp_index_dest)
      {
        // result[arg2] = arg1
        x86_gen_index_dest(context, assign);
      }
      else
      {
        // result = arg1 op arg2
        x86_gen_bin_expr(context, assign);
      }
    }
    else
    {
      if(assign->op == eIrOp_deref_source)
      {
        // result = ^arg1
        x86_gen_deref_source(context, assign);
      }
      else if(assign->op == eIrOp_deref_dest)
      {
        // ^result = arg1
        x86_gen_deref_dest(context, assign);
      }
      else if(assign->op == eIrOp_address_of)
      {
        // result = &arg1
        x86_gen_address_of(context, assign);
      }
      else
      {
        // result = op arg1
        x86_gen_unr_expr(context, assign);
      }
    }
  }
  else
  {
    // result = arg1
    x86_gen_equal(context, assign);
  }

  // IMPORTANT: The updated live-info is used in the find_least_used_register() function.
  // The update must be done *here*, so that the when the next statement is processed,
  // a particular object will have the live-info of the previous last statement it appeared in.
  // As a bonus consequence, the objects of 'result', 'arg1' and 'arg2' are automatically excluded from the find_least_used_register() search function.
  update_object_live_info(result, arg1, arg2);

  discard_all_unused_args(context, assign);
}

// goto L
void x86_gen_goto(X86Context* context, IrLabel* goto_label)
{
  save_all_registers(context, false);

  X86Stmt* jump_stmt = x86_new_stmt(context, eX86Stmt_jmp);
  jump_stmt->operand1 = x86_make_id_operand(goto_label->name);
}

// if arg1 relop arg2 goto L
void x86_gen_cond_goto(X86Context* context, struct IrStmt_cond_goto* cond_goto)
{
  IrArg* arg1 = cond_goto->arg1;
  IrArg* arg2 = cond_goto->arg2;

  save_all_registers(context, false);

  if(!is_object_in_register(context, arg1->object))
  {
    X86Location* arg1_loc = get_best_available_register(context, arg1->object->ty);
    x86_load_object(context, arg1->object, arg1_loc);
  }

  X86Stmt* cmp_stmt = 0;
  if(types_are_equal(arg1->object->ty, basic_type_int)
    || types_are_equal(arg1->object->ty, basic_type_char)
    || types_are_equal(arg1->object->ty, basic_type_bool))
  {
    cmp_stmt = x86_new_stmt(context, eX86Stmt_cmp);
  }
  else if(types_are_equal(arg1->object->ty, basic_type_float))
  {
    cmp_stmt = x86_new_stmt(context, eX86Stmt_ucomiss);
  }
  else assert(0);

  cmp_stmt->operand1 = x86_make_object_operand(context, arg1->object);
  cmp_stmt->operand2 = x86_make_object_operand(context, arg2->object);

  X86Stmt* jump_stmt = x86_new_stmt(context, conv_ir_op_to_x86_opcode(cond_goto->relop, arg1->object->ty));
  jump_stmt->operand1 = x86_make_id_operand(cond_goto->label->name);
}

void x86_gen_call(X86Context* context, struct IrStmt_call* call)
{
  save_all_registers(context, true);

  /* sub esp, #param_size */
  X86Stmt* stmt = x86_new_stmt(context, eX86Stmt_sub);
  stmt->operand1 = x86_make_register_operand(context, &context->esp);
  stmt->operand2 = x86_make_int_constant_operand(call->param_scope->allocd_size);

  /* call #proc_name */
  stmt = x86_new_stmt(context, eX86Stmt_call);

  stmt->operand1 = x86_make_id_operand(call->name);

  if(call->is_extern)
  {
    /* add esp, #retvar_size */
    stmt = x86_new_stmt(context, eX86Stmt_add);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    stmt->operand2 = x86_make_int_constant_operand(call->retvar->allocd_size);

    if(!types_are_equal(call->retvar->ty, basic_type_void))
    {
      clean_register_all_levels(context, &context->eax);
      set_exclusive_object_location(context, call->retvar, &context->eax);
    }
  }
  else
  {
    /* add esp, #param_size */
    stmt = x86_new_stmt(context, eX86Stmt_add);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    stmt->operand2 = x86_make_int_constant_operand(call->param_scope->allocd_size);
  }
}

void x86_gen_basic_block(X86Context* context, BasicBlock* bb)
{
  for(int i = 0; i < bb->stmt_count; i++)
  {
    IrStmt* ir_stmt = bb->stmt_array[i];

    switch(ir_stmt->kind)
    {
      case eIrStmt_assign:
      {
        x86_gen_assign(context, &ir_stmt->assign);
      }
      break;

      case eIrStmt_goto:
      {
        // goto L
        x86_gen_goto(context, ir_stmt->goto_label);
      }
      break;

      case eIrStmt_cond_goto:
      {
        // if arg1 relop arg2 goto L
        x86_gen_cond_goto(context, &ir_stmt->cond_goto);
      }
      break;

      case eIrStmt_call:
      {
        x86_gen_call(context, &ir_stmt->call);
      }
      break;

      case eIrStmt_return:
      case eIrStmt_nop:
      break;

      default: assert(0);
    }
  }
}

void x86_gen_extern_proc(X86Context* context, AstNode* proc)
{
  /* extern #proc_name:proc */
  X86Stmt* stmt = x86_new_stmt(context, eX86Stmt_extern_proc);
  stmt->operand1 = x86_make_id_operand(proc->proc.decorated_name);
}

void x86_gen_proc(X86Context* context, AstNode* proc)
{
  if((proc->modifier & eModifier_extern) != 0)
  {
    x86_gen_extern_proc(context, proc);
  }
  else
  {
    List* basic_blocks = proc->proc.basic_blocks;
    ListItem* li = basic_blocks->first;
    BasicBlock* first_bb = KIND(li, eList_basic_block)->basic_block;

    /* #proc_name: */
    X86Stmt* stmt = x86_new_stmt(context, eX86Stmt_label);
    stmt->operand1 = x86_make_id_operand(proc->proc.name);

    /* push ebp */
    stmt = x86_new_stmt(context, eX86Stmt_push);
    stmt->operand1 = x86_make_register_operand(context, &context->ebp);

    /* mov ebp, esp */
    stmt = x86_new_stmt(context, eX86Stmt_mov);
    stmt->operand1 = x86_make_register_operand(context, &context->ebp);
    stmt->operand2 = x86_make_register_operand(context, &context->esp);

    /* sub esp, #frame_size */
    Scope* proc_scope = proc->proc.scope;
    stmt = x86_new_stmt(context, eX86Stmt_sub);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    stmt->operand2 = x86_make_int_constant_operand(proc_scope->allocd_size);

    x86_gen_basic_block(context, first_bb);
    save_all_registers(context, true);

    for(li = li->next;
        li;
        li = li->next)
    {
      BasicBlock* bb = KIND(li, eList_basic_block)->basic_block;

      if(bb->label)
      {
        X86Stmt* label_stmt = x86_new_stmt(context, eX86Stmt_label);
        label_stmt->operand1 = x86_make_id_operand(bb->label->name);
      }

      x86_gen_basic_block(context, bb);
      save_all_registers(context, true);
    }

    /* mov esp, ebp */
    stmt = x86_new_stmt(context, eX86Stmt_mov);
    stmt->operand1 = x86_make_register_operand(context, &context->esp);
    stmt->operand2 = x86_make_register_operand(context, &context->ebp);

    /* pop ebp */
    stmt = x86_new_stmt(context, eX86Stmt_pop);
    stmt->operand1 = x86_make_register_operand(context, &context->ebp);

    /* ret */
    x86_new_stmt(context, eX86Stmt_ret);
  }
}

void x86_init_registers(X86Context* context)
{
  context->register_count = 0;

  /* eax */
  X86Location* loc = &context->eax;
  loc->kind = eX86Location_eax;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* al */
  X86Location* sub = &context->al;
  loc->sub[0] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_al;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* ah */
  sub = &context->ah;
  loc->sub[1] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_ah;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* ebx */
  loc = &context->ebx;
  loc->kind = eX86Location_ebx;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* bl */
  sub = &context->bl;
  loc->sub[0] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_bl;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* bh */
  sub = &context->bh;
  loc->sub[1] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_bh;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* ecx */
  loc = &context->ecx;
  loc->kind = eX86Location_ecx;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* cl */
  sub = &context->cl;
  loc->sub[0] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_cl;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* ch */
  sub = &context->ch;
  loc->sub[1] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_ch;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* edx */
  loc = &context->edx;
  loc->kind = eX86Location_edx;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* dl */
  sub = &context->dl;
  loc->sub[0] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_dl;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* dh */
  sub = &context->dh;
  loc->sub[1] = sub;
  sub->parent = loc;
  sub->kind = eX86Location_dh;
  sub->type = basic_type_char;
  context->registers._[context->register_count++] = sub;
  init_list(&sub->occupants, arena, eList_symbol);

  /* esi */
  loc = &context->esi;
  loc->kind = eX86Location_esi;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* edi */
  loc = &context->edi;
  loc->kind = eX86Location_edi;
  loc->type = basic_type_int;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* memory */
  loc = &context->memory;
  *loc = (X86Location){0};
  loc->kind = eX86Location_memory;
  loc->type = basic_type_void;
  init_list(&loc->occupants, arena, eList_symbol);

  /* ebp */
  loc = &context->ebp;
  loc->kind = eX86Location_ebp;
  loc->type = basic_type_int;
  init_list(&loc->occupants, arena, eList_symbol);

  /* esp */
  loc = &context->esp;
  loc->kind = eX86Location_esp;
  loc->type = basic_type_int;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm0 */
  loc = &context->xmm0;
  loc->kind = eX86Location_xmm0;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm1 */
  loc = &context->xmm1;
  loc->kind = eX86Location_xmm1;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm2 */
  loc = &context->xmm2;
  loc->kind = eX86Location_xmm2;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm3 */
  loc = &context->xmm3;
  loc->kind = eX86Location_xmm3;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm4 */
  loc = &context->xmm4;
  loc->kind = eX86Location_xmm4;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm5 */
  loc = &context->xmm5;
  loc->kind = eX86Location_xmm5;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm6 */
  loc = &context->xmm6;
  loc->kind = eX86Location_xmm6;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  /* xmm7 */
  loc = &context->xmm7;
  loc->kind = eX86Location_xmm7;
  loc->type = basic_type_float;
  context->registers._[context->register_count++] = loc;
  init_list(&loc->occupants, arena, eList_symbol);

  assert(context->register_count <= sizeof_array(context->registers._));
}

void x86_write_data_bytes(String* text, uint8* p_data, int data_size)
{
  int i;
  for(i = 0; i < data_size - 1; i++)
  {
    str_printf(text, "0%xh,", p_data[i]);
  }

  if(data_size > 0)
  {
    str_printf(text, "0%xh", p_data[i]);
  }

  str_printfln(text, "");
}

void x86_write_static_data_text(String* text, Scope* scope)
{
  for(ListItem* li = scope->decl_syms.first;
      li;
      li = li->next)
  {
    Symbol* object = KIND(li, eList_symbol)->symbol;
    if(object->storage_space == eStorageSpace_static && object->allocd_size > 0)
    {
      if(object->data)
      {
        str_printf(text, "byte ");

        x86_write_data_bytes(text, (uint8*)object->data, object->ty->width);

        int padding_size = object->allocd_size - object->ty->width;
        if(padding_size > 0)
        {
          str_printfln(text, "byte %d dup(?)", padding_size);
        }
      }
      else
      {
        str_printfln(text, "byte %d dup(?)", object->allocd_size);
      }
    }
  }
}

void x86_gen_module(IrContext* ir_context, X86Context* x86_context, AstNode* module)
{
  List* procs = &module->module.procs;
  for(ListItem* li = procs->first;
      li;
      li = li->next)
  {
    AstNode* proc = KIND(li, eList_ast_node)->ast_node;

    partition_to_basic_blocks(ir_context->stmt_arena, proc);
    x86_gen_proc(x86_context, proc);
  }
}

void x86_gen(IrContext* ir_context, X86Context* x86_context, AstNode* module, String* x86_text)
{
  x86_gen_module(ir_context, x86_context, module);

  DEBUG_print_ir_code(arena, &module->module.procs, "./out.ir");

  str_init(x86_text, push_arena(&arena, 1*MEGABYTE));
  str_printfln(x86_text, ".686");
  str_printfln(x86_text, ".xmm");
  str_printfln(x86_text, ".model flat, C");
  str_printfln(x86_text, ".stack 4096");
  str_printfln(x86_text, ".data");
  str_printfln(x86_text, "static_area label byte");
  str_printfln(x86_text, "align %d", ir_context->data_alignment);

  x86_write_static_data_text(x86_text, module->module.scope);

  str_printfln(x86_text, ".code");
  str_printfln(x86_text, "public startup");

  for(int i = 0; i < x86_context->stmt_count; i++)
  {
    x86_print_stmt(x86_text, &x86_context->stmt_array[i]);
  }

  str_printfln(x86_text, "end");
}
