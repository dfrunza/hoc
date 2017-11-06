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

void compute_area_locations(List* pre_fp_areas, List* post_fp_areas)
{
  int fp = compute_area_loc(0, pre_fp_areas);
  compute_area_loc(fp, post_fp_areas);
  fixup_area_loc(fp, pre_fp_areas);
  fixup_area_loc(fp, post_fp_areas);
}

int compute_decl_areas(Scope* scope, SymbolKind* kind_set, List* areas)
{
  int size = 0;
  for(SymbolKind* kind = kind_set;
      *kind != Symbol_None;
      kind++)
  {
    for(ListItem* list_item = scope->decls[*kind]->first;
        list_item;
        list_item = list_item->next)
    {
      Type* type = ITEM(list_item, symbol)->type;
      DataArea* area = ITEM(list_item, symbol)->data_area = mem_push_struct(arena, DataArea);
      area->kind = DataArea_data;
      area->size = type->width;

      size += area->size;
      append_list_elem(areas, area, List_data_area);
    }
  }
  return size;
}

int compute_occur_areas(Scope* scope, SymbolKind* kind_set, List* areas)
{
  int size = 0;
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
          append_list_elem(areas, link, List_data_area);
          size += link->size;
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
  return size;
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
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      DataArea* base_offset = mem_push_struct(arena, DataArea);
      base_offset->size = 1; // null pointer location
      scope->local_area_size = base_offset->size;
      append_list_elem(post_fp_areas, base_offset, List_data_area);

      scope->local_area_size = compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_str, Symbol_None}, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_proc)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      compute_decl_areas(scope, (SymbolKind[]){Symbol_ret_var, Symbol_formal_arg, Symbol_None}, pre_fp_areas);
      scope->link_area_size = compute_occur_areas(scope, (SymbolKind[]){Symbol_var, Symbol_str, Symbol_None}, pre_fp_areas);

      DataArea* ctrl_area = mem_push_struct(arena, DataArea);
      ctrl_area->size = 3*4; // FP, SP, IP
      append_list_elem(pre_fp_areas, ctrl_area, List_data_area);

      scope->local_area_size = compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_None}, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_block || scope->kind == Scope_loop)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      scope->link_area_size = compute_occur_areas(scope, (SymbolKind[]){Symbol_var, Symbol_str, Symbol_None}, pre_fp_areas);

      DataArea* ctrl_area = mem_push_struct(arena, DataArea);
      ctrl_area->size = 4; // FP
      append_list_elem(pre_fp_areas, ctrl_area, List_data_area);

      scope->local_area_size = compute_decl_areas(scope, (SymbolKind[]){Symbol_var, Symbol_None}, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
  }
}


