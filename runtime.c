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

void compute_locals_area_size(Scope* scope, List* local_areas)
{
  for(ListItem* list_item = scope->decls[Symbol_var]->first;
      list_item;
      list_item = list_item->next)
  {
    Type* type = ITEM(list_item, symbol)->type;
    DataArea* area = ITEM(list_item, symbol)->data_area = mem_push_struct(arena, DataArea);
    area->kind = DataArea_var;
    area->size = type->width;

    scope->data_area_size += area->size;
    append_list_elem(local_areas, area, List_data_area);
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
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      DataArea* base_offset = mem_push_struct(arena, DataArea);
      base_offset->size = 1; // null ptr
      scope->data_area_size = base_offset->size;
      append_list_elem(post_fp_areas, base_offset, List_data_area);

      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_module)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);

      compute_locals_area_size(scope, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_proc)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);
      scope->data_area_size = 0;

      Type* ret_type = ITEM(scope->decls[Symbol_ret_var]->first, symbol)->type;
      DataArea* ret_area = ITEM(scope->decls[Symbol_ret_var]->first, symbol)->data_area = mem_push_struct(arena, DataArea);
      ret_area->kind = DataArea_var;
      ret_area->size = ret_type->width;

      scope->data_area_size += ret_area->size;
      append_list_elem(pre_fp_areas, ret_area, List_data_area);

      for(ListItem* list_item = scope->decls[Symbol_formal_arg]->first;
          list_item;
          list_item = list_item->next)
      {
        Type* arg_type = ITEM(list_item, symbol)->type;
        DataArea* arg_area = ITEM(list_item, symbol)->data_area = mem_push_struct(arena, DataArea);
        arg_area->kind = DataArea_var;
        arg_area->size = arg_type->width;
        append_list_elem(pre_fp_areas, arg_area, List_data_area);
      }

      compute_locals_area_size(scope, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
    else if(scope->kind == Scope_block)
    {
      List* pre_fp_areas = new_list(arena, List_data_area);
      List* post_fp_areas = new_list(arena, List_data_area);
      List* access_links = new_list(arena, List_data_area);
      scope->access_links_size = 0;

      for(ListItem* list_item = scope->occurs[Symbol_var]->first;
          list_item;
          list_item = list_item->next)
      {
        int decl_scope_offset = ITEM(list_item, symbol)->decl_scope_offset;
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
            link = mem_push_struct(arena, DataArea);
            link->kind = DataArea_link;
            link->decl_scope_offset = decl_scope_offset;
            link->size = 4; // size of an int
            append_list_elem(access_links, link, List_data_area);
            append_list_elem(pre_fp_areas, link, List_data_area);
            scope->access_links_size += link->size;
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

      DataArea* old_fp = mem_push_struct(arena, DataArea);
      old_fp->size = 4; // size of an int
      append_list_elem(pre_fp_areas, old_fp, List_data_area);

      compute_locals_area_size(scope, post_fp_areas);
      compute_area_locations(pre_fp_areas, post_fp_areas);
    }
  }
}


