int compute_area_loc(int sp, List* areas)
{
  for(ListItem* list_item = areas->first;
      list_item;
      list_item = list_item->next)
  {
    DataArea* area = ITEM(list_item, data_area);
    area->loc = sp;
    assert(area->size >= 0);
    sp += area->size;
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
  }
}

void compute_area_locations(DataArea* pre_fp_area, DataArea* post_fp_area)
{
  int fp = compute_area_loc(0, pre_fp_area->subareas);
  compute_area_loc(fp, post_fp_area->subareas);
  fixup_area_loc(fp, pre_fp_area->subareas);
  fixup_area_loc(fp, post_fp_area->subareas);
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
      Type* type = ITEM(list_item, symbol)->type;
      DataArea* data = ITEM(list_item, symbol)->data_area = mem_push_struct(arena, DataArea);
      data->kind = DataArea_data;
      data->size = type->width;

      decl_area->size += data->size;
      append_list_elem(decl_area->subareas, data, List_data_area);
    }
  }
}

void compute_occur_areas(Scope* scope, SymbolKind* kind_set, DataArea* occur_area)
{
  //List* access_links = new_list(arena, List_data_area);
  for(SymbolKind* kind = kind_set;
      *kind != Symbol_None;
      kind++)
  {
    for(ListItem* list_item = scope->occurs[*kind]->first;
        list_item;
        list_item = list_item->next)
    {
      int decl_scope_offset = ITEM(list_item, symbol)->decl_scope_offset;
      if(decl_scope_offset > 0)
      {
        // non-local
        DataArea* link = 0;
        for(ListItem* list_item = scope->access_links->first;
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
          link = mem_push_struct(arena, DataArea);
          link->kind = DataArea_link;
          link->decl_scope_offset = decl_scope_offset;
          link->size = 4; // size of an int
          append_list_elem(scope->access_links, link, List_data_area);
          append_list_elem(occur_area->subareas, link, List_data_area);
          occur_area->size += link->size;
        }
        ITEM(list_item, symbol)->data_area = link;
      }
      else if(decl_scope_offset == 0)
      {
        // local
        Symbol* decl_sym = ITEM(list_item, symbol)->decl;
        ITEM(list_item, symbol)->data_area = decl_sym->data_area;
      }
      else
        assert(0);
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
      scope->pre_fp_area.subareas = new_list(arena, List_data_area);
      scope->post_fp_area.subareas = new_list(arena, List_data_area);

      DataArea* null_area = mem_push_struct(arena, DataArea);
      int32* null = null_area->data = mem_push_struct(arena, int32);
      *null = 0;
      null_area->size = sizeof(int32);

      //scope->local_area_size = null_area->size;
      append_list_elem(scope->pre_fp_area.subareas, null_area, List_data_area);
      scope->pre_fp_area.size += null_area->size;

      compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_str, Symbol_None}, &scope->post_fp_area);
      compute_area_locations(&scope->pre_fp_area, &scope->post_fp_area);
    }
    else if(scope->kind == Scope_proc)
    {
      scope->pre_fp_area.subareas = new_list(arena, List_data_area);
      scope->post_fp_area.subareas = new_list(arena, List_data_area);

      compute_decl_areas(scope, (SymbolKind[]){Symbol_return_var, Symbol_formal_arg, Symbol_None}, &scope->pre_fp_area);
      compute_occur_areas(scope, (SymbolKind[]){Symbol_var, Symbol_str, Symbol_None}, &scope->pre_fp_area);

      DataArea* ctrl_area = mem_push_struct(arena, DataArea);
      ctrl_area->size = 3*4; // FP, SP, IP
      append_list_elem(scope->pre_fp_area.subareas, ctrl_area, List_data_area);
      scope->pre_fp_area.size += ctrl_area->size;

      compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_None}, &scope->post_fp_area);
      compute_area_locations(&scope->pre_fp_area, &scope->post_fp_area);
    }
    else if(scope->kind == Scope_block || scope->kind == Scope_loop)
    {
      scope->pre_fp_area.subareas = new_list(arena, List_data_area);
      scope->post_fp_area.subareas = new_list(arena, List_data_area);

      compute_occur_areas(scope, (SymbolKind[]){Symbol_var, Symbol_str, Symbol_return_var, Symbol_None}, &scope->pre_fp_area);

      DataArea* ctrl_area = mem_push_struct(arena, DataArea);
      ctrl_area->size = 4; // FP
      append_list_elem(scope->pre_fp_area.subareas, ctrl_area, List_data_area);
      scope->pre_fp_area.size += ctrl_area->size;

      compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_None}, &scope->post_fp_area);
      compute_area_locations(&scope->pre_fp_area, &scope->post_fp_area);
    }
    else
      assert(0);
  }
}


