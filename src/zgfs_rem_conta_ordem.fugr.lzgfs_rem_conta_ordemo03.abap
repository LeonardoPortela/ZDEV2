*----------------------------------------------------------------------*
***INCLUDE LZGFS_REM_CONTA_ORDEMO03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0120 OUTPUT.

  SET PF-STATUS 'ZLOGPROC'.
  SET TITLEBAR 'ZLOGPROC'.

  PERFORM init_alv3.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv3.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog3.
  PERFORM sort_table3.

* PERFORM toolbar_alv3.

  l_stable-row          = abap_true.
  l_stable-col          = abap_true.

  w_layout-zebra        = abap_false.
*   w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar    = abap_false.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
      it_sort              = t_sort
      it_outtab            = t_logproc.

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
    SET HANDLER : m_event_handler->toolbar FOR g_grid.
    SET HANDLER : m_event_handler->user_command FOR g_grid.
  ENDIF.

  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid,
               lcl_event_handler=>on_double_click  FOR g_grid.

*   CALL METHOD g_grid->refresh_table_display
*     EXPORTING
*       is_stable = l_stable.

ENDFORM.                    " init_tree

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM sort_table3.

  FREE: t_sort.

  CLEAR w_sort.
  w_sort-fieldname = 'VBELN'.
  w_sort-spos      = 1.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

  CLEAR w_sort.
  w_sort-fieldname = 'ETAPA_DESC'.
  w_sort-spos      = 2.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

  CLEAR w_sort.
  w_sort-fieldname = 'DATA'.
  w_sort-spos      = 3.
  w_sort-down      = 'X'.
  APPEND w_sort   TO t_sort.

  CLEAR w_sort.
  w_sort-fieldname = 'HORA'.
  w_sort-spos      = 4.
  w_sort-down      = 'X'.
  APPEND w_sort   TO t_sort.

ENDFORM.

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog3.

  FREE: t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'STATUS'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 04.
  ls_fieldcatalog-dd_outlen = 04.
  ls_fieldcatalog-coltext   = ''.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'VBELN'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Ordem Venda'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'ETAPA_DESC'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-dd_outlen = 20.
  ls_fieldcatalog-coltext   = 'Etapa Proc.'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'DOC_GERADO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Doc.Gerado'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'DATA'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Data'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'HORA'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Hora'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'USUARIO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 7.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-dd_outlen = 20.
  ls_fieldcatalog-coltext   = 'Usuário'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGPROC'.
  ls_fieldcatalog-fieldname = 'MSGV1'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 8.
  ls_fieldcatalog-outputlen = 80.
  ls_fieldcatalog-dd_outlen = 80.
  ls_fieldcatalog-coltext   = 'Mensagem'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv3.

  FREE: pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
*
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.

* IF ( p_escala = abap_false ) OR l_edit = abap_false.
*   APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
*   APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.
* ENDIF.

ENDFORM.
