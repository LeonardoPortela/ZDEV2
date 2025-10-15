*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
  if wl_cell is not initial.
*    REFRESH TL_CELL.
*
*    APPEND WL_CELL TO TL_CELL.

*    DATA: IT_SELECTED_ROWS_0102 TYPE LVC_T_ROW,
*          WA_SELECTED_ROWS_0102 TYPE LVC_S_ROW.
*
*    WA_SELECTED_ROWS_0102 = 1.
*    APPEND WA_SELECTED_ROWS_0102 TO IT_SELECTED_ROWS_0102.
*
*    CALL METHOD OBJ_ALV_0140->SET_SELECTED_ROWS
*      EXPORTING
*        IT_INDEX_ROWS = IT_SELECTED_ROWS_0102.
  endif.

  set pf-status '0100'.
  set titlebar  '0100'.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
module pbo_0100 output.
  data: custom_tree_name(30)    type c value 'CUSTOM_TREE',
        obj_tree_event_receiver type ref to lcl_tree_event_receiver,
        obj_picture             type ref to cl_gui_picture,
        lt_events               type cntl_simple_events,
        url(255)                type c,
        l_logo                  type sdydo_value,
        l_event                 type cntl_simple_event.

  create object obj_tree_event_receiver.

  if obj_custom_tree is initial.

    create object obj_docking
      exporting
        side      = cl_gui_docking_container=>dock_at_left
        extension = 208
        repid     = sy-repid
        dynnr     = '0100'.

    create object obj_custom_tree
      exporting
        container_name              = custom_tree_name
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.

    create object obj_alv_tree
      exporting
        parent              = obj_docking
        node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
        item_selection      = ''
        no_html_header      = 'X'
        no_toolbar          = 'X'.

    data: l_header type treev_hhdr.
    perform build_header changing l_header.

    wl_fcat-fieldname = ' '.
    wl_fcat-no_out    = 'X'.
    wl_fcat-key       = ' '.
    append wl_fcat to gt_fcat_0100.
    clear wl_fcat.

    call method obj_alv_tree->set_table_for_first_display
      exporting
        is_hierarchy_header = l_header
*      I_SAVE              = 'A'
      changing
        it_outtab           = gt_menu_tree
        it_fieldcatalog     = gt_fcat_0100.

    perform create_hierarchy.

    clear l_event.
    l_event-eventid    = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    append l_event to lt_events.

    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    append l_event to lt_events.
    clear l_event.

    call method obj_alv_tree->set_registered_events
      exporting
        events                    = lt_events
      exceptions
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.
  endif.

  set handler obj_tree_event_receiver->handle_double_click for obj_alv_tree.
  call method obj_alv_tree->frontend_update.
endmodule.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
form create_hierarchy.
  data: p_relat_key type lvc_nkey,
        p_pai_key   type lvc_nkey,
        p_filho_key type lvc_nkey,
        l_node_text type lvc_value.

  clear gt_menu_tree.

  wl_menu_tree-node_pai         = 'Dados Complementares'.
  wl_menu_tree-node_image_pai   = icon_real_estate_object.
*  wl_menu_tree-node_image_filho = icon_transport_proposal.
  wl_menu_tree-node_filho       = 'Incluir/alterar dados'.
  append wl_menu_tree to gt_menu_tree.

  wl_menu_tree-node_pai         = 'Controle de Impostos'.
  wl_menu_tree-node_image_pai   = icon_wizard.
*  wl_menu_tree-node_image_filho = icon_generate.
  wl_menu_tree-node_filho       = 'Gerar contas a pagar'.
  append wl_menu_tree to gt_menu_tree.

  wl_menu_tree-node_pai         = 'Relatórios'.
  wl_menu_tree-node_image_pai   = icon_biw_report.
  wl_menu_tree-node_filho       = 'Dados Complementares'.
  append wl_menu_tree to gt_menu_tree.

  wl_menu_tree-node_filho       = 'Dados Impostos'.
  append wl_menu_tree to gt_menu_tree.

  loop at gt_menu_tree into wl_menu_tree.
    if not wl_menu_tree is initial.

      on change of wl_menu_tree-node_pai.
        perform add_pai using space
                              wl_menu_tree-node_image_pai
                              wl_menu_tree-node_pai
                              changing p_pai_key.
      endon.

      on change of wl_menu_tree-node_filho.
        perform add_filho using p_pai_key
                                wl_menu_tree-node_image_filho
                                wl_menu_tree-node_filho
                                changing p_filho_key.
      endon.

    endif.
  endloop.
endform.                    "CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*&      Form  ADD_PAI
*&---------------------------------------------------------------------*
form add_pai using    p_relat_key type lvc_nkey
                      p_node_image type tv_image
                      p_node_pai  type char50
             changing p_pai_key   type lvc_nkey.

  data: l_node_text type lvc_value,
        l_layout_node type lvc_s_layn.

  l_node_text = p_node_pai.
  l_layout_node-n_image = p_node_image.

  call method obj_alv_tree->add_node
    exporting
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_node_layout   = l_layout_node
    importing
      e_new_node_key   = p_pai_key.

endform.                    "ADD_PAI


*&---------------------------------------------------------------------*
*&      Form  ADD_FILHO
*&---------------------------------------------------------------------*
form add_filho using  p_pai_key    type lvc_nkey
                      p_node_image type tv_image
                      p_node_filho type char50
             changing p_filho_key  type lvc_nkey.

  data: l_node_text   type lvc_value,
        l_layout_node type lvc_s_layn.

  l_node_text   = p_node_filho.
  l_layout_node-n_image = p_node_image.

  call method obj_alv_tree->add_node
    exporting
      i_relat_node_key = p_pai_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_node_layout   = l_layout_node
    importing
      e_new_node_key   = p_filho_key.

endform.                    "ADD_FILHO

*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
form build_header changing p_header type treev_hhdr.
  p_header-heading = 'Controle de Frotas'.
  p_header-tooltip = 'Controle de Frotas'.
  p_header-width   = 20.
endform.                    "BUILD_HEADER
