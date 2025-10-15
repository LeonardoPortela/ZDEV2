*----------------------------------------------------------------------*
*   INCLUDE BCALV_TREE_EVENT_RECEIVER                                  *
*----------------------------------------------------------------------*
class lcl_tree_event_receiver definition.

  public section.

  methods handle_item_double_click
      for event item_double_click of cl_gui_alv_tree
          importing node_key
                    fieldname.

endclass.

class lcl_tree_event_receiver implementation.
  method handle_item_double_click.
    data: lt_selected_nodes type lvc_t_nkey,
          l_node_key        type lvc_nkey,
          l_fieldname       type lvc_fname.

    refresh: lt_selected_nodes.
    call method tree1->get_selected_nodes
    changing
      ct_selected_nodes = lt_selected_nodes.
    if lt_selected_nodes[] is initial.
      call method tree1->get_selected_item
      importing
        e_selected_node = l_node_key
        e_fieldname     = l_fieldname.
    else.
      read table lt_selected_nodes into l_node_key index 1.
    endif.
    if not l_node_key is initial.
      perform f_tree_double_click using l_node_key.
    endif.
  endmethod.
endclass.
