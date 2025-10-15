*----------------------------------------------------------------------*
***INCLUDE LZ_FIO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET TITLEBAR 'Z_FI_ESTRAT'.
  SET PF-STATUS 'Z_FI_ESTRAT'.

  PERFORM init_alv.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

MODULE status_0200 OUTPUT.

  SET TITLEBAR 'Z_LOG_APROV'.
  SET PF-STATUS 'Z_FI_ESTRAT'.

  PERFORM init_alv2.
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
  PERFORM toolbar_alv.

  l_stable-row          = abap_true.
  l_stable-col          = abap_true.

  w_layout-zebra        = abap_false.
*   w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = it_estrat.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

*   CALL METHOD g_grid->refresh_table_display
*     EXPORTING
*       is_stable = l_stable.

ENDFORM.                    " init_tree

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv2.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog2.
  PERFORM toolbar_alv.

  l_stable-row          = abap_true.
  l_stable-col          = abap_true.

  w_layout-zebra        = abap_false.
*   w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = it_logaprov.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

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
  ls_fieldcatalog-tabname   = 'T_ESTRAT'.
  ls_fieldcatalog-fieldname = 'NIVEL_APROV'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Nivel Aprov'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ESTRAT'.
  ls_fieldcatalog-fieldname = 'BRANCH'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Loc.Negócio'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ESTRAT'.
  ls_fieldcatalog-fieldname = 'USNAM'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Usuário'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ESTRAT'.
  ls_fieldcatalog-fieldname = 'NOME'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 50.
  ls_fieldcatalog-dd_outlen = 50.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Nome Completo'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ESTRAT'.
  ls_fieldcatalog-fieldname = 'DEPARTAMENTO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-dd_outlen = 20.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Departamento'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

ENDFORM.

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog2.

  FREE: t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'SEQ_LCTO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 14.
  ls_fieldcatalog-dd_outlen = 14.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Sequencia'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'OPERACAO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Operação'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'TIPO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 13.
  ls_fieldcatalog-dd_outlen = 13.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Tipo Execução'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'NIVEL_APROV'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Nivel Aprov'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'BRANCH'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Loc.Negócio'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'USNAM'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Aprovador'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'DATA_ATUAL'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 7.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Data Aprovação'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'HORA_ATUAL'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 8.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Hora Aprovação'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_LOGAPROV'.
  ls_fieldcatalog-fieldname = 'USUARIO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 9.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Usuário'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN '&OK'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0200 INPUT.

  CASE ok_code.
    WHEN '&OK'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
