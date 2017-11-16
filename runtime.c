DataArea* new_data_area(MemoryArena* arena, DataAreaKind kind)
{
  DataArea* result = mem_push_struct(arena, DataArea);
  result->kind = kind;
  return result;
}

int compute_area_loc(int sp, DataArea* area)
{
  area->loc = sp;
  sp = area->loc + area->size;
  if(area->subareas)
  {
    int loc = area->loc;
    for(ListItem* list_item = area->subareas->first;
        list_item;
        list_item = list_item->next)
    {
      DataArea* subarea = ITEM(list_item, data_area);
      subarea->loc = loc;
      loc = compute_area_loc(loc, subarea);
    }
    assert(loc == sp);
  }
  return sp;
}

void fixup_area_loc(int fp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    area->loc = area->loc - fp;

    if(area->subareas)
    {
      fixup_area_loc(fp, area->subareas);
    }
  }
}

void compute_area_locations(List* pre_fp_areas, List* post_fp_areas)
{
  int sp = 0;
  int size = 0;
  for(ListItem* list_item = pre_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    sp = compute_area_loc(sp, area);
    size += area->size;
  }
  assert(sp == size);

  int fp = sp;
  for(ListItem* list_item = post_fp_areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    sp = compute_area_loc(sp, area);
    size += area->size;
  }
  assert(sp == size);

  fixup_area_loc(fp, pre_fp_areas);
  fixup_area_loc(fp, post_fp_areas);
}

void compute_decl_areas(Scope* scope, SymbolKind* kind_set, DataArea* decl_area)
{
  for(SymbolKind* kind = kind_set;
      *kind != Symbol_None;
      kind++)
  {
    for(ListItem* list_item = scope->decls[*kind]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* symbol = ITEM(list_item, symbol);
      DataArea* area = symbol->data_area = new_data_area(arena, DataArea_data);
      area->size = symbol->type->width;
      area->data = symbol->data;

      decl_area->size += area->size;
      append_list_elem(decl_area->subareas, area, List_data_area);
    }
  }
}

void compute_occur_areas(Scope* scope, SymbolKind* kind_set, DataArea* link)
{
  //List* access_links = link_area->subareas;
  for(SymbolKind* kind = kind_set;
      *kind != Symbol_None;
      kind++)
  {
    for(ListItem* list_item = scope->occurs[*kind]->first;
        list_item;
        list_item = list_item->next)
    {
      Symbol* occur_sym = ITEM(list_item, symbol);
      Symbol* decl_sym = occur_sym->decl;

      int decl_scope_offset = occur_sym->decl_scope_offset;
      if(decl_scope_offset > 0)
      {
        link->decl_scope_offset = decl_scope_offset;
        link->size = 4; // size of an int
        occur_sym->data_area = link;
      }
      else if(decl_scope_offset == 0)
      {
        occur_sym->data_area = decl_sym->data_area;
      }
#if 0
      int decl_scope_offset = occur_sym->decl_scope_offset;
      if(decl_scope_offset > 0)
      {
        // non-local
        DataArea* link = 0;
        for(ListItem* list_item = access_links->first;
            list_item;
            list_item = list_item->next)
        {
          link = ITEM(list_item, data_area);
          if(link->decl_scope_offset == decl_scope_offset)
          {
            break;
          }
          link = 0;
        }
        if(!link)
        {
          link = new_data_area(arena, DataArea_link);
          link->decl_scope_offset = decl_scope_offset;
          link->size = 4; // size of an int
          append_list_elem(access_links, link, List_data_area);
          link_area->size += link->size;
        }
        occur_sym->data_area = link;
      }
      else if(decl_scope_offset == 0)
      {
        // local
        Symbol* decl_sym = occur_sym->decl;
        occur_sym->data_area = decl_sym->data_area;
      }
      else
        assert(0);
#endif
    }
  }
}

void build_runtime()
{
  for(ListItem* list_item = symbol_table->scopes->first;
      list_item;
      list_item = list_item->next)
  {
    Scope* scope = ITEM(list_item, scope);
    if(scope->kind == Scope_global)
    {
      ;//skip
    }
    else if(scope->kind == Scope_module)
    {
      scope->pre_fp_areas = new_list(arena, List_data_area);
      scope->post_fp_areas = new_list(arena, List_data_area);

      DataArea* null_area = new_data_area(arena, DataArea_data);
      append_list_elem(scope->pre_fp_areas, null_area, List_data_area);
      int32* null = null_area->data = mem_push_struct(arena, int32);
      *null = 0;
      null_area->size = sizeof(*null);

      DataArea* ctrl_area = &scope->ctrl_area;
      append_list_elem(scope->pre_fp_areas, ctrl_area, List_data_area);
      ctrl_area->size = 4; // FP

      DataArea* local_area = &scope->local_area;
      local_area->subareas = new_list(arena, List_data_area);
      append_list_elem(scope->post_fp_areas, local_area, List_data_area);
      compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_None}, local_area);

      compute_area_locations(scope->pre_fp_areas, scope->post_fp_areas);
    }
    else if(scope->kind == Scope_proc)
    {
      scope->pre_fp_areas = new_list(arena, List_data_area);
      scope->post_fp_areas = new_list(arena, List_data_area);

      DataArea* ret_area = &scope->ret_area;
      ret_area->subareas = new_list(arena, List_data_area);
      append_list_elem(scope->pre_fp_areas, ret_area, List_data_area);
      compute_decl_areas(scope, (SymbolKind[]){Symbol_ret_var, Symbol_None}, ret_area);

      DataArea* args_area = &scope->args_area;
      args_area->subareas = new_list(arena, List_data_area);
      append_list_elem(scope->pre_fp_areas, args_area, List_data_area);
      compute_decl_areas(scope, (SymbolKind[]){Symbol_formal_arg, Symbol_None}, args_area);

      DataArea* link_area = &scope->link_area;
      //link_area->subareas = new_list(arena, List_data_area);
      append_list_elem(scope->pre_fp_areas, link_area, List_data_area);
      compute_occur_areas(scope, (SymbolKind[])
          {Symbol_var, Symbol_ret_var, Symbol_formal_arg, Symbol_None}, link_area);

      DataArea* ctrl_area = &scope->ctrl_area;
      append_list_elem(scope->pre_fp_areas, ctrl_area, List_data_area);
      ctrl_area->size = 3*4; // FP, SP, IP

      DataArea* local_area = &scope->local_area;
      local_area->subareas = new_list(arena, List_data_area);
      append_list_elem(scope->post_fp_areas, local_area, List_data_area);
      compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_None}, local_area);

      compute_area_locations(scope->pre_fp_areas, scope->post_fp_areas);
    }
    else if(scope->kind == Scope_block || scope->kind == Scope_loop)
    {
      scope->pre_fp_areas = new_list(arena, List_data_area);
      scope->post_fp_areas = new_list(arena, List_data_area);

      DataArea* link_area = &scope->link_area;
      //link_area->subareas = new_list(arena, List_data_area);
      append_list_elem(scope->pre_fp_areas, link_area, List_data_area);
      compute_occur_areas(scope, (SymbolKind[])
          {Symbol_var, Symbol_ret_var, Symbol_formal_arg, Symbol_None}, link_area);

      DataArea* ctrl_area = &scope->ctrl_area;
      append_list_elem(scope->pre_fp_areas, ctrl_area, List_data_area);
      ctrl_area->size = 4; // FP

      DataArea* local_area = &scope->local_area;
      local_area->subareas = new_list(arena, List_data_area);
      append_list_elem(scope->post_fp_areas, local_area, List_data_area);
      compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_None}, local_area);

      compute_area_locations(scope->pre_fp_areas, scope->post_fp_areas);
    }
    else
      assert(0);
  }
}


