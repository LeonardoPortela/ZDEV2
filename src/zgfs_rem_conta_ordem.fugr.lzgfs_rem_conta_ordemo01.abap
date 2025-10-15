*----------------------------------------------------------------------*
***INCLUDE LZGFS_REM_CONTA_ORDEMO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  FREE: t_exctab.

  IF l_elimina = abap_false.
    MOVE '&DESMARCA' TO w_exctab-fcode.
    APPEND w_exctab  TO t_exctab.
  ENDIF.

  SET PF-STATUS 'ZSDORDEM' EXCLUDING t_exctab.
  SET TITLEBAR 'ZSDORDEM'.

  PERFORM init_alv.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog.
* PERFORM toolbar_alv.

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
*     it_sort              = lt_sort
      it_outtab            = t_saida.

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
FORM build_fieldcatalog.

  FREE: t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'DT_EMISSAO'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
  ls_fieldcatalog-ref_field = 'DT_EMISSAO'.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Dt.Emissão'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'NUMERO'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
  ls_fieldcatalog-ref_field = 'NUMERO'.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 9.
  ls_fieldcatalog-dd_outlen = 9.
  ls_fieldcatalog-coltext   = 'Nr.NF-e'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'SERIE'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
  ls_fieldcatalog-ref_field = 'SERIE'.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Série NF-e'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

*-CS2024000522-21.06.2024-JT-#143588-inicio
  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'FORNE_CNPJ'.
* ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
* ls_fieldcatalog-ref_field = 'FORNE_CNPJ'.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 18.
  ls_fieldcatalog-dd_outlen = 18.
  ls_fieldcatalog-coltext   = 'CNPJ Fornecedor'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'FORNE_IE'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
  ls_fieldcatalog-ref_field = 'FORNE_IE'.
  ls_fieldcatalog-col_pos   = 5.    "*-CS2024000522-21.06.2024-JT-#143588
  ls_fieldcatalog-outputlen = 22.
  ls_fieldcatalog-dd_outlen = 22.
  ls_fieldcatalog-coltext   = 'Inscr.Est.Fornecedor'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.
*-CS2024000522-21.06.2024-JT-#143588-fim

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'PROD_DESCRICAO'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_ITM'.
  ls_fieldcatalog-ref_field = 'PROD_DESCRICAO'.
  ls_fieldcatalog-col_pos   = 6.    "*-CS2024000522-21.06.2024-JT-#143588
  ls_fieldcatalog-outputlen = 30.
  ls_fieldcatalog-dd_outlen = 30.
  ls_fieldcatalog-coltext   = 'Material'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'PROD_QTD_COMERCI'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_ITM'.
  ls_fieldcatalog-ref_field = 'PROD_QTD_COMERCI'.
  ls_fieldcatalog-col_pos   = 7.    "*-CS2024000522-21.06.2024-JT-#143588
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Quantidade'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'VL_TOTAL'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
  ls_fieldcatalog-ref_field = 'VL_TOTAL'.
  ls_fieldcatalog-col_pos   = 8.     "*-CS2024000522-21.06.2024-JT-#143588
  ls_fieldcatalog-outputlen = 18.
  ls_fieldcatalog-dd_outlen = 18.
  ls_fieldcatalog-coltext   = 'Valor'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'CHAVE_NFE'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
  ls_fieldcatalog-ref_field = 'CHAVE_NFE'.
  ls_fieldcatalog-col_pos   = 9.    "*-CS2024000522-21.06.2024-JT-#143588
  ls_fieldcatalog-outputlen = 55.
  ls_fieldcatalog-dd_outlen = 55.
  ls_fieldcatalog-coltext   = 'Chave NF-e'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

*-CS2024000522-18.07.2024-JT-#143588-inicio
  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'AGENTE_FRETE'.
  ls_fieldcatalog-ref_table = 'LFA1'.
  ls_fieldcatalog-ref_field = 'LIFNR'.
  ls_fieldcatalog-col_pos   = 10.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Ag.Frete'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'PLACA'.
* ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
* ls_fieldcatalog-ref_field = 'CHAVE_NFE'.
  ls_fieldcatalog-col_pos   = 11.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Placa'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'VLR_UNIT_FRETE'.
* ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
* ls_fieldcatalog-ref_field = 'CHAVE_NFE'.
  ls_fieldcatalog-col_pos   = 12.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Vlr.Unit.Frete'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_SAIDA'.
  ls_fieldcatalog-fieldname = 'CHAVE_CTE'.
  ls_fieldcatalog-ref_table = 'ZIB_NFE_DIST_TER'.
  ls_fieldcatalog-ref_field = 'CHAVE_NFE'.
  ls_fieldcatalog-col_pos   = 13.
  ls_fieldcatalog-outputlen = 55.
  ls_fieldcatalog-dd_outlen = 55.
  ls_fieldcatalog-coltext   = 'Chave CTE'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.
*-CS2024000522-18.07.2024-JT-#143588-fim

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv.

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
