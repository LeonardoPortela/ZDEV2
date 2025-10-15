*----------------------------------------------------------------------*
***INCLUDE LZGSD_SELOS_SOJA_SAPO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  FREE: t_exctab.

  SET TITLEBAR 'ZSDF001'.
  SET PF-STATUS 'ZSDF001' EXCLUDING t_exctab.

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
  PERFORM toolbar_alv.

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
      it_outtab            = t_zpmx0055.

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

*  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid,
*               lcl_event_handler=>on_double_click  FOR g_grid.

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
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'DATA_REG'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Data Registro'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'HORA_REG'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Hora Registro'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'USER_REG'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Usuário Registro'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'ACAO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Ação'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'WERKS'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 06.
  ls_fieldcatalog-dd_outlen = 06.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Centro'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'SAFRA'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-outputlen = 06.
  ls_fieldcatalog-dd_outlen = 06.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Safra'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'MATNR'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 7.
  ls_fieldcatalog-outputlen = 18.
  ls_fieldcatalog-dd_outlen = 18.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Material'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'SELO'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 8.
  ls_fieldcatalog-outputlen = 30.
  ls_fieldcatalog-dd_outlen = 30.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Imagem'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'ID_LOTE'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 9.
  ls_fieldcatalog-outputlen = 06.
  ls_fieldcatalog-dd_outlen = 06.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'ID Lote'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'LOTE'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 10.
  ls_fieldcatalog-outputlen = 08.
  ls_fieldcatalog-dd_outlen = 08.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Lote'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'CICLO_DESC'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 11.
  ls_fieldcatalog-outputlen = 20.
  ls_fieldcatalog-dd_outlen = 20.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Ciclo'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'DIA'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 12.
  ls_fieldcatalog-outputlen = 06.
  ls_fieldcatalog-dd_outlen = 06.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Dias'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'VALIDADE'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 13.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-edit      = abap_false.
  ls_fieldcatalog-coltext   = 'Validade'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'BLOQUEAR'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 14.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-checkbox  = abap_true.
  ls_fieldcatalog-coltext   = 'Bloquear?'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_ZPMX0055'.
  ls_fieldcatalog-fieldname = 'IMPRIMIR'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 15.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-checkbox  = abap_true.
  ls_fieldcatalog-coltext   = 'Imprimir?'.
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
