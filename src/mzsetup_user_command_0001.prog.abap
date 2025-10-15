*----------------------------------------------------------------------*
***INCLUDE MZSETUP_USER_COMMAND_0001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0001 input.

  case ok_principal.
      "WHEN .
      "WHEN .
      "WHEN OTHERS.
  endcase.

endmodule.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  CONFIGURA_TELA_MESTRE  OUTPUT
*&---------------------------------------------------------------------*
*       Colocar novas telas para alv tree view
*----------------------------------------------------------------------*
module configura_tela_mestre output.

  types: item_table_type like standard table of mtreeitm
         with default key.

  data: url(255),
        node_table type treev_ntab,
        item_table type item_table_type,
        events     type cntl_simple_events,
        event      type cntl_simple_event,
        vg_dynnr   type sydynnr,
        vg_subrc   type sy-subrc.

  set pf-status 'PFPRINC'.
  set titlebar  'TL0001'.

  if init_principal is initial.

    create object g_application.

    create object docking
      exporting
        name      = 'CONTAINER_DOC'
        repid     = sy-cprog
        dynnr     = sy-dynnr
        side      = docking->dock_at_left
        extension = 180
        ratio     = 30.

    create object g_tree
      exporting
        parent                      = docking
        node_selection_mode         = cl_gui_list_tree=>node_sel_mode_single
        item_selection              = c_x
        with_headers                = ' '
      exceptions
        cntl_system_error           = 1
        create_error                = 2
        failed                      = 3
        illegal_node_selection_mode = 4
        lifetime_error              = 5.

    " button click
    event-eventid = cl_gui_list_tree=>eventid_item_double_click.
    event-appl_event = 'X'.
    append event to events.

    call method g_tree->set_registered_events
      exporting
        events                    = events
      exceptions
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

    set handler g_application->handle_item_double_click for g_tree.

    perform const_node_and_item_table using node_table item_table.

    call method g_tree->add_nodes_and_items
      exporting
        node_table                     = node_table
        item_table                     = item_table
        item_table_structure_name      = 'MTREEITM'
      exceptions
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.

    init_principal = c_x.

  endif.

  if g_item_name is not initial.

    vg_dynnr = g_item_name.
    perform verifica_autorizacao using vg_dynnr vg_subrc.
    if vg_subrc is initial.
      call method docking->link
        exporting
          repid = sy-cprog
          dynnr = vg_dynnr.

      leave to screen vg_dynnr.
    endif.
  endif.

endmodule.                 " CONFIGURA_TELA_MESTRE  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CARREGA_FIGURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_URL  text
*----------------------------------------------------------------------*
form carrega_figura  changing p_url.

  data query_table like w3query occurs 1 with header line.
  data html_table like w3html occurs 1.
  data return_code like  w3param-ret_code.
  data content_type like  w3param-cont_type.
  data content_length like  w3param-cont_len.
  data pic_data like w3mime occurs 0.
  data pic_size type i.

  refresh query_table.
  query_table-name = '_OBJECT_ID'.
  query_table-value = 'ENJOYSAP_LOGO'.
  append query_table.

  call function 'WWW_GET_MIME_OBJECT'
    tables
      query_string        = query_table
      html                = html_table
      mime                = pic_data
    changing
      return_code         = return_code
      content_type        = content_type
      content_length      = content_length
    exceptions
      object_not_found    = 1
      parameter_not_found = 2
      others              = 3.
  if sy-subrc = 0.
    pic_size = content_length.
  endif.

  call function 'DP_CREATE_URL'
    exporting
      type     = 'image'
      subtype  = cndp_sap_tab_unknown
      size     = pic_size
      lifetime = cndp_lifetime_transaction
    tables
      data     = pic_data
    changing
      url      = p_url
    exceptions
      others   = 1.

endform.                    " CARREGA_FIGURA

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0001_exit input.

  leave program.

endmodule.                 " USER_COMMAND_0001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONST_NODE_AND_ITEM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form const_node_and_item_table
  using
    node_table type treev_ntab
    item_table type item_table_type.

  data: node       type treev_node,
        item       type mtreeitm,
        cg_root    type c length 1,
        node_child type tv_nodekey,
        node_key   type tv_nodekey,
        it_root    type table of zsapmzsetup_root with header line,
        it_root_c  type table of zsapmzsetup_root with header line,
        it_root_p  type table of zsapmzsetup_root with header line,
        it_telas   type table of zsapmzsetup with header line.

  select * into table it_root from zsapmzsetup_root.

  select * into table it_telas from zsapmzsetup.

  move it_root[] to it_root_c[].
  move it_root[] to it_root_p[].

  loop at it_root.

    clear node.
    if it_root-cd_root_pai is initial.
      move it_root-cd_root to node_key.
      node-node_key = node_key.
      node-hidden   = space.
      node-disabled = space.
      node-isfolder = 'X'.
      append node to node_table.
    else.
      read table it_root_c with key cd_root = it_root-cd_root_pai.
      move it_root-cd_root   to node_child.
      move it_root_c-cd_root to node_key.
      node-node_key  = node_child.
      node-relatkey  = node_key.
      node-relatship = cl_gui_list_tree=>relat_last_child.
      node-isfolder  = 'X'.
      append node to node_table.
    endif.

    "Texto da Pasta
    clear item.
    item-node_key  = node-node_key.
    item-item_name = node-node_key.
    item-class     = cl_gui_list_tree=>item_class_text.
    item-alignment = cl_gui_list_tree=>align_auto.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = it_root-tx_root.
    append item to item_table.

    loop at it_telas where cd_root eq it_root-cd_root.

      move it_telas-cd_dynnr to node_child.
      move it_root-cd_root   to node_key.
      node-node_key  = node_child.
      node-relatkey  = node_key.
      node-relatship = cl_gui_list_tree=>relat_last_child.
      node-isfolder  = space.
      append node to node_table.

      clear item.
      item-node_key  = node-node_key.
      item-item_name = node-node_key.
      item-class     = cl_gui_list_tree=>item_class_text.
      item-alignment = cl_gui_list_tree=>align_auto.
      item-font      = cl_gui_list_tree=>item_font_prop.
      item-text      = it_telas-tx_dynnr.
      append item to item_table.

    endloop.

  endloop.

endform.                    " CONST_NODE_AND_ITEM_TABLE

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_AUTORIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_DYNNR  text
*----------------------------------------------------------------------*
form verifica_autorizacao  using  p_dynnr type sy-dynnr p_subrc type sy-subrc.

  data: wa_zsapmzsetup type zsapmzsetup.

  p_subrc = 1.

  select single * into wa_zsapmzsetup from zsapmzsetup where cd_dynnr eq p_dynnr.
  if sy-subrc is initial.

    authority-check object wa_zsapmzsetup-cp_autorizacao id wa_zsapmzsetup-cp_autorizacao field c_x.
    if not sy-subrc is initial.
      message s000 with 'Sem acesso para' wa_zsapmzsetup-tx_dynnr.
    else.
      p_subrc = 0.
    endif.
  endif.

endform.                    " VERIFICA_AUTORIZACAO
