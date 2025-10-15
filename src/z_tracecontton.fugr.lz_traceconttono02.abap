*----------------------------------------------------------------------*
***INCLUDE LZ_TRACECONTTONO02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  CLEAR ok_code.
  SET PF-STATUS 'ZLOGERRO'.
  SET TITLEBAR 'ZLOGERRO'.

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
  w_layout-no_toolbar   = abap_false.
  w_layout-cwidth_opt   = abap_true.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
      it_sort              = t_sort
      it_outtab            = t_saida.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  IF m_event_handler IS INITIAL.
    CREATE OBJECT m_event_handler.
    SET HANDLER : m_event_handler->toolbar FOR g_grid.
    SET HANDLER : m_event_handler->user_command FOR g_grid.
  ENDIF.

  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid,
               lcl_event_handler=>on_double_click  FOR g_grid.

ENDFORM.                    " init_tree

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM sort_table3.

  FREE: t_sort.

ENDFORM.

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog3.

  DATA: t_fcat     TYPE  slis_t_fieldcat_alv. "Fieldcatalog

  FREE: t_fieldcatalog.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZSDT0295'
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT t_fcat             INTO DATA(w_fcat).
    MOVE-CORRESPONDING w_fcat  TO w_fieldcatalog.
    MOVE w_fcat-seltext_l      TO w_fieldcatalog-scrtext_l.
    MOVE w_fcat-seltext_m      TO w_fieldcatalog-scrtext_m.
    MOVE w_fcat-seltext_s      TO w_fieldcatalog-scrtext_s.
    MOVE w_fcat-seltext_l      TO w_fieldcatalog-coltext.

    CASE w_fcat-fieldname.
      WHEN 'LINHA'.
        w_fieldcatalog-col_pos   = 1.
        w_fieldcatalog-scrtext_l = 'Linha Excel'.
        w_fieldcatalog-scrtext_m = 'Linha Excel'.
        w_fieldcatalog-scrtext_s = 'Linha Excel'.
        w_fieldcatalog-coltext   = 'Linha Excel'.
      WHEN 'LOTE'.
        w_fieldcatalog-col_pos   = 2.
        w_fieldcatalog-scrtext_l = 'Lote'.
        w_fieldcatalog-scrtext_m = 'Lote'.
        w_fieldcatalog-scrtext_s = 'Lote'.
        w_fieldcatalog-coltext   = 'Lote'.
      WHEN 'TIPO'.
        w_fieldcatalog-col_pos   = 3.
        w_fieldcatalog-scrtext_l = 'Tipo'.
        w_fieldcatalog-scrtext_m = 'Tipo'.
        w_fieldcatalog-scrtext_s = 'Tipo'.
        w_fieldcatalog-coltext   = 'Tipo'.
      WHEN 'COD_MATERIAL'.
        w_fieldcatalog-col_pos   = 4.
        w_fieldcatalog-ref_table = 'MARA'.
        w_fieldcatalog-ref_field = 'MATNR'.
        w_fieldcatalog-scrtext_l = 'Material'.
        w_fieldcatalog-scrtext_m = 'Material'.
        w_fieldcatalog-scrtext_s = 'Material'.
        w_fieldcatalog-coltext   = 'Material'.
      WHEN 'QTD_FARDOS'.
        w_fieldcatalog-col_pos   = 5.
        w_fieldcatalog-scrtext_l = 'Qtd.Fardos'.
        w_fieldcatalog-scrtext_m = 'Qtd.Fardos'.
        w_fieldcatalog-scrtext_s = 'Qtd.Fardos'.
        w_fieldcatalog-coltext   = 'Qtd.Fardos'.
      WHEN 'PESO_LOTE'.
        w_fieldcatalog-col_pos   = 6.
        w_fieldcatalog-scrtext_l = 'Peso Lote'.
        w_fieldcatalog-scrtext_m = 'Peso Lote'.
        w_fieldcatalog-scrtext_s = 'Peso Lote'.
        w_fieldcatalog-coltext   = 'Peso Lote'.
      WHEN 'MOTIVO'.
        w_fieldcatalog-col_pos   = 7.
        w_fieldcatalog-scrtext_l = 'Motivo'.
        w_fieldcatalog-scrtext_m = 'Motivo'.
        w_fieldcatalog-scrtext_s = 'Motivo'.
        w_fieldcatalog-coltext   = 'Motivo'.
      WHEN 'COD_FILIAL'.
        w_fieldcatalog-col_pos   = 8.
        w_fieldcatalog-ref_table = 'T001W'.
        w_fieldcatalog-ref_field = 'WERKS'.
        w_fieldcatalog-scrtext_l = 'Filial'.
        w_fieldcatalog-scrtext_m = 'Filial'.
        w_fieldcatalog-scrtext_s = 'Filial'.
        w_fieldcatalog-coltext   = 'Filial'.
      WHEN 'CONTRATO'.
        w_fieldcatalog-col_pos   = 9.
        w_fieldcatalog-scrtext_l = 'Contrato'.
        w_fieldcatalog-scrtext_m = 'Contrato'.
        w_fieldcatalog-scrtext_s = 'Contrato'.
        w_fieldcatalog-coltext   = 'Contrato'.
      WHEN 'TAM_FARDO'.
        w_fieldcatalog-col_pos   = 10.
        w_fieldcatalog-scrtext_l = 'Tam.Fardo'.
        w_fieldcatalog-scrtext_m = 'Tam.Fardo'.
        w_fieldcatalog-scrtext_s = 'Tam.Fardo'.
        w_fieldcatalog-coltext   = 'Tam.Fardo'.
      WHEN 'DATA_TAKEUP'.
        w_fieldcatalog-col_pos   = 11.
        w_fieldcatalog-scrtext_l = 'Data TakeUp'.
        w_fieldcatalog-scrtext_m = 'Data TakeUp'.
        w_fieldcatalog-scrtext_s = 'Data TakeUp'.
        w_fieldcatalog-coltext   = 'Data TakeUp'.
      WHEN 'EMPRESA'.
        w_fieldcatalog-col_pos   = 12.
        w_fieldcatalog-scrtext_l = 'Empresa'.
        w_fieldcatalog-scrtext_m = 'Empresa'.
        w_fieldcatalog-scrtext_s = 'Empresa'.
        w_fieldcatalog-coltext   = 'Empresa'.
      WHEN 'FORNECEDOR'.
        w_fieldcatalog-col_pos   = 13.
        w_fieldcatalog-ref_table = 'LFA1'.
        w_fieldcatalog-ref_field = 'LIFNR'.
        w_fieldcatalog-scrtext_l = 'Fornecedor'.
        w_fieldcatalog-scrtext_m = 'Fornecedor'.
        w_fieldcatalog-scrtext_s = 'Fornecedor'.
        w_fieldcatalog-coltext   = 'Fornecedor'.
      WHEN 'MENSAGEM'.
        w_fieldcatalog-col_pos   = 14.
        w_fieldcatalog-scrtext_l = 'Mensagem'.
        w_fieldcatalog-scrtext_m = 'Mensagem'.
        w_fieldcatalog-scrtext_s = 'Mensagem'.
        w_fieldcatalog-coltext   = 'Mensagem'.
      WHEN 'ERRO'.
        w_fieldcatalog-no_out    = abap_true.
    ENDCASE.

    APPEND w_fieldcatalog      TO t_fieldcatalog.
  ENDLOOP.

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
