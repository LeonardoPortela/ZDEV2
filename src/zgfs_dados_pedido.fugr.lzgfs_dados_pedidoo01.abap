*----------------------------------------------------------------------*
***INCLUDE LZGFS_DADOS_PEDIDOO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  FREE: t_exctab_po.

  IF vg_display EQ abap_true.
    MOVE 'GERAR' TO w_exctab_po-fcode.
    APPEND w_exctab_po  TO t_exctab_po.
  ENDIF.

  SET PF-STATUS 'ZMMPOP0100' EXCLUDING t_exctab_po.
  SET TITLEBAR  'ZMMPOT0100'.

  PERFORM init_alv.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form init_alv
*&---------------------------------------------------------------------*
FORM init_alv .

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog.
  PERFORM toolbar_alv.

  l_stable-row          = abap_true.
  l_stable-col          = abap_true.

  w_layout-zebra        = abap_false.
  IF vg_display IS INITIAL.
    w_layout-edit         = abap_true. " Makes all Grid editable
  ENDIF.
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = t_function
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = gt_dados_pedidos.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

  IF m_event_handler IS INITIAL.
    CREATE OBJECT m_event_handler.
    SET HANDLER : m_event_handler->toolbar         FOR g_grid.
    SET HANDLER : m_event_handler->user_command    FOR g_grid.
    SET HANDLER : m_event_handler->on_data_changed FOR g_grid.
  ENDIF.

*SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid.
*              lcl_event_handler=>on_double_click  FOR g_grid.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = l_stable.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_fieldcatalog
*&---------------------------------------------------------------------*
FORM build_fieldcatalog .

  FREE: t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'GT_DADOS_PEDIDOS'.
  ls_fieldcatalog-fieldname = 'EBELN'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Pedido'.
  IF vg_display IS INITIAL.
    ls_fieldcatalog-edit      = abap_true.
  ENDIF.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'GT_DADOS_PEDIDOS'.
  ls_fieldcatalog-fieldname = 'EBELP'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 06.
  ls_fieldcatalog-dd_outlen = 06.
  ls_fieldcatalog-coltext   = 'Item pedido'.
  IF vg_display IS INITIAL.
    ls_fieldcatalog-edit      = abap_true.
  ENDIF.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'GT_DADOS_PEDIDOS'.
  ls_fieldcatalog-fieldname = 'MENGE'.
  ls_fieldcatalog-ref_table = 'EKPO'.
  ls_fieldcatalog-ref_field = 'MENGE'.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Quantidade remessa'.
  IF vg_display IS INITIAL.
    ls_fieldcatalog-edit      = abap_true.
  ENDIF.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form toolbar_alv
*&---------------------------------------------------------------------*
FORM toolbar_alv .

  FREE: t_function.
*  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.

  APPEND cl_gui_alv_grid=>mc_fc_check          TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc       TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_refresh        TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut        TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy       TO t_function.
  APPEND cl_gui_alv_grid=>mc_mb_paste          TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo       TO t_function.
*  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row   TO t_function.
*  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO t_function.
*  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO t_function.
*  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO t_function.


ENDFORM.
