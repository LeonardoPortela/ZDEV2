*----------------------------------------------------------------------*
***INCLUDE ZMMR118_STATUS_LOG.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9004 OUTPUT.
  SET PF-STATUS 'PF9004'.
  SET TITLEBAR '9004'.
  "

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.
  DATA: gs_scroll_col2 TYPE lvc_s_col,
        gs_scroll_row2 TYPE lvc_s_roid.

  DATA: it_logs TYPE zde_nfe_dist_log_alv_t,
        wa_logs TYPE zde_nfe_dist_log_alv.

  CLEAR: it_log_alv.
  IF obj_nfeclass IS INITIAL.
    CREATE OBJECT obj_nfeclass.
    obj_nfeclass->nfe_inbound( ).
  ENDIF.

  DATA(lc_cabec) = obj_nfe->get_cabecalho_nota( ).
  it_logs = obj_nfeclass->at_nfe_inbound->get_log_proc_nfe( p_chave = lc_cabec-chave_nfe ).

  LOOP AT it_logs INTO wa_logs.
    MOVE-CORRESPONDING wa_logs TO wa_log_alv.
    APPEND wa_log_alv TO it_log_alv.
  ENDLOOP.

  IF obg_conteiner_log IS INITIAL.
    CREATE OBJECT obg_conteiner_log
      EXPORTING
        container_name = 'CC_LOG'.

    CREATE OBJECT ctl_alv_nfe_hist
      EXPORTING
        i_parent = obg_conteiner_log.

    PERFORM fill_it_fieldcatalog2.

*   Fill info for layout variant2
    PERFORM fill_gs_variant2.

    CALL METHOD ctl_alv_nfe_hist->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout2
        is_variant      = gs_variant2
        i_save          = 'A'
      " it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog = it_fieldcatalog2
        it_outtab       = it_log_alv.

  ELSE.
    CALL METHOD ctl_alv_nfe_hist->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004 INPUT.
 CASE okcode.
    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
