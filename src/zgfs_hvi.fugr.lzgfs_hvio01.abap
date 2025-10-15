*----------------------------------------------------------------------*
***INCLUDE LZGFS_HVIO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZLOGHVI'.
  SET TITLEBAR 'ZLOGHVI'.

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
      i_structure_name       = 'ZMMT0027'
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
      WHEN 'SAFRA'.
        w_fieldcatalog-scrtext_l = 'Safra'.
        w_fieldcatalog-scrtext_m = 'Safra'.
        w_fieldcatalog-scrtext_s = 'Safra'.
        w_fieldcatalog-coltext   = 'Safra'.
      WHEN 'VARIEDADE'.
        w_fieldcatalog-scrtext_l = 'Variedade'.
        w_fieldcatalog-scrtext_m = 'Variedade'.
        w_fieldcatalog-scrtext_s = 'Variedade'.
        w_fieldcatalog-coltext   = 'Variedade'.
      WHEN 'TALHAO'.
        w_fieldcatalog-scrtext_l = 'Talhão'.
        w_fieldcatalog-scrtext_m = 'Talhão'.
        w_fieldcatalog-scrtext_s = 'Talhão'.
        w_fieldcatalog-coltext   = 'Talhão'.
      WHEN 'FAR_UHML'.
        w_fieldcatalog-scrtext_l = 'UHML'.
        w_fieldcatalog-scrtext_m = 'UHML'.
        w_fieldcatalog-scrtext_s = 'UHML'.
        w_fieldcatalog-coltext   = 'UHML'.
      WHEN 'FAR_UI'.
        w_fieldcatalog-scrtext_l = 'UI'.
        w_fieldcatalog-scrtext_m = 'UI'.
        w_fieldcatalog-scrtext_s = 'UI'.
        w_fieldcatalog-coltext   = 'UI'.
      WHEN 'FAR_STR'.
        w_fieldcatalog-scrtext_l = 'STR'.
        w_fieldcatalog-scrtext_m = 'STR'.
        w_fieldcatalog-scrtext_s = 'STR'.
        w_fieldcatalog-coltext   = 'STR'.
      WHEN 'FAR_ELG'.
        w_fieldcatalog-scrtext_l = 'ELG'.
        w_fieldcatalog-scrtext_m = 'ELG'.
        w_fieldcatalog-scrtext_s = 'ELG'.
        w_fieldcatalog-coltext   = 'ELG'.
      WHEN 'FAR_MIC'.
        w_fieldcatalog-scrtext_l = 'MIC'.
        w_fieldcatalog-scrtext_m = 'MIC'.
        w_fieldcatalog-scrtext_s = 'MIC'.
        w_fieldcatalog-coltext   = 'MIC'.
      WHEN 'FAR_RD'.
        w_fieldcatalog-scrtext_l = 'RD'.
        w_fieldcatalog-scrtext_m = 'RD'.
        w_fieldcatalog-scrtext_s = 'RD'.
        w_fieldcatalog-coltext   = 'RD'.
      WHEN 'FAR_B'.
        w_fieldcatalog-scrtext_l = 'B'.
        w_fieldcatalog-scrtext_m = 'B'.
        w_fieldcatalog-scrtext_s = 'B'.
        w_fieldcatalog-coltext   = 'B'.
      WHEN 'FAR_CG'.
        w_fieldcatalog-scrtext_l = 'CG'.
        w_fieldcatalog-scrtext_m = 'CG'.
        w_fieldcatalog-scrtext_s = 'CG'.
        w_fieldcatalog-coltext   = 'CG'.
      WHEN 'FAR_TCNT'.
        w_fieldcatalog-scrtext_l = 'TCNT'.
        w_fieldcatalog-scrtext_m = 'TCNT'.
        w_fieldcatalog-scrtext_s = 'TCNT'.
        w_fieldcatalog-coltext   = 'TCNT'.
      WHEN 'FAR_TAREA'.
        w_fieldcatalog-scrtext_l = 'TAREA'.
        w_fieldcatalog-scrtext_m = 'TAREA'.
        w_fieldcatalog-scrtext_s = 'TAREA'.
        w_fieldcatalog-coltext   = 'TAREA'.
      WHEN 'FAR_LEAF'.
        w_fieldcatalog-scrtext_l = 'LEAF'.
        w_fieldcatalog-scrtext_m = 'LEAF'.
        w_fieldcatalog-scrtext_s = 'LEAF'.
        w_fieldcatalog-coltext   = 'LEAF'.
      WHEN 'FAR_B'.
        w_fieldcatalog-scrtext_l = 'B'.
        w_fieldcatalog-scrtext_m = 'B'.
        w_fieldcatalog-scrtext_s = 'B'.
        w_fieldcatalog-coltext   = 'B'.
      WHEN 'FAR_MR'.
        w_fieldcatalog-scrtext_l = 'MR'.
        w_fieldcatalog-scrtext_m = 'MR'.
        w_fieldcatalog-scrtext_s = 'MR'.
        w_fieldcatalog-coltext   = 'MR'.
      WHEN 'FAR_SFIW'.
        w_fieldcatalog-scrtext_l = 'SFIW'.
        w_fieldcatalog-scrtext_m = 'SFIW'.
        w_fieldcatalog-scrtext_s = 'SFIW'.
        w_fieldcatalog-coltext   = 'SFIW'.
      WHEN 'FAR_SCI'.
        w_fieldcatalog-scrtext_l = 'SCI'.
        w_fieldcatalog-scrtext_m = 'SCI'.
        w_fieldcatalog-scrtext_s = 'SCI'.
        w_fieldcatalog-coltext   = 'SCI'.
      WHEN 'FAR_CSP'.
        w_fieldcatalog-scrtext_l = 'CSP'.
        w_fieldcatalog-scrtext_m = 'CSP'.
        w_fieldcatalog-scrtext_s = 'CSP'.
        w_fieldcatalog-coltext   = 'CSP'.
      WHEN 'FAR_PERIODO'.
        w_fieldcatalog-scrtext_l = 'PERIODO'.
        w_fieldcatalog-scrtext_m = 'PERIODO'.
        w_fieldcatalog-scrtext_s = 'PERIODO'.
        w_fieldcatalog-coltext   = 'PERIODO'.
      WHEN 'ADQUIRIDO_TERC'.
        w_fieldcatalog-scrtext_l = 'ADQUIRIDO_TERC'.
        w_fieldcatalog-scrtext_m = 'ADQUIRIDO_TERC'.
        w_fieldcatalog-scrtext_s = 'ADQUIRIDO_TERC'.
        w_fieldcatalog-coltext   = 'ADQUIRIDO_TERC'.
        w_fieldcatalog-checkbox  = abap_true.
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
