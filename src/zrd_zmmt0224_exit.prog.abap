*&---------------------------------------------------------------------*
*& Report  ZRD_ZMMT0224_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0224_exit.

FORM f_exit_zmmt0224_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0224 TYPE zmmt0224,
        lv_interval LIKE nriv.

  CLEAR: wl_zmmt0224.

  CALL FUNCTION 'NUMBER_GET_INFO'
    EXPORTING
      nr_range_nr        = '01'
      object             = 'ZMMT0224'
    IMPORTING
      interval           = lv_interval
    EXCEPTIONS
      interval_not_found = 1
      object_not_found   = 2
      OTHERS             = 3.

  IF sy-subrc = 0.
    wl_zmmt0224-id_modo_aplicacao = lv_interval-nrlevel + 1.
  ENDIF.

  wl_zmmt0224-date_create = sy-datum.
  wl_zmmt0224-time_create = sy-uzeit.
  wl_zmmt0224-user_create = sy-uname.

  MOVE-CORRESPONDING wl_zmmt0224 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0224_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0224 TYPE zmmt0224.

  CLEAR: wl_zmmt0224.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0224.
*
*  CLEAR: p_error.
*
  IF wl_zmmt0224-modo_aplicacao IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Informar Modo Aplicação' TYPE 'S'.
    EXIT.
  ENDIF.

  wl_zmmt0224-date_create = sy-datum.
  wl_zmmt0224-time_create = sy-uzeit.
  wl_zmmt0224-user_create = sy-uname.

ENDFORM.

FORM f_exit_zmmt0224_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zmmt0224 TYPE zmmt0224,
        lv_num      TYPE numc5.

  CLEAR: wl_zmmt0224.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0224.

  IF sy-ucomm EQ 'NOVO'.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZMMT0224'
      IMPORTING
        number      = lv_num
      EXCEPTIONS
        OTHERS      = 8.

    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    wl_zmmt0224-id_modo_aplicacao = lv_num.
  ENDIF.

  wl_zmmt0224-date_create = sy-datum.
  wl_zmmt0224-time_create = sy-uzeit.
  wl_zmmt0224-user_create = sy-uname.

  MOVE-CORRESPONDING wl_zmmt0224 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0224_0004 CHANGING p_saida TYPE any.

  DATA: wl_zmmt0224_out TYPE zmmt0224_out.

  CLEAR: wl_zmmt0224_out.

  MOVE-CORRESPONDING p_saida TO wl_zmmt0224_out.

  MOVE-CORRESPONDING wl_zmmt0224_out TO p_saida.

ENDFORM.

FORM f_exit_zmmt0224_0008  CHANGING p_col_pos
                                    p_ref_tabname
                                    p_ref_fieldname
                                    p_tabname
                                    p_field
                                    p_scrtext_l
                                    p_outputlen
                                    p_edit
                                    p_sum
                                    p_emphasize
                                    p_just
                                    p_hotspot
                                    p_f4
                                    p_check.

  IF p_ref_tabname = 'ZMMT0224_OUT' AND
     p_field       = 'ID_MODO_APLICACAO'.
    p_scrtext_l    = 'ID Modo Aplicacao'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0224_OUT' AND
     p_field       = 'MODO_APLICACAO'.
    p_scrtext_l    = 'Modo Aplicação'.
    p_outputlen    = 150.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0224_OUT' AND
     p_field       = 'USER_CREATE'.
    p_scrtext_l    = 'Usuário'.
    p_outputlen    = 20.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0224_OUT' AND
     p_field       = 'DATE_CREATE'.
    p_scrtext_l    = 'Data'.
    p_outputlen    = 12.
  ENDIF.

  IF p_ref_tabname = 'ZMMT0224_OUT' AND
     p_field       = 'TIME_CREATE'.
    p_scrtext_l    = 'Hora'.
    p_outputlen    = 12.
  ENDIF.

ENDFORM.

FORM  f_exit_zmmt0224_0009 TABLES pt_excl_toolbar
                           USING p_db_tab.
*
  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  FREE: it_excl_toolbar.

*  wa_excl_toolbar-code = 'Modificar'.
*  APPEND wa_excl_toolbar  TO it_excl_toolbar.
*
*  pt_excl_toolbar[] = it_excl_toolbar[].

ENDFORM.

FORM f_exit_zmmt0224_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZMMT0224'
      tabfirst = 'X'.

ENDFORM.
