*&---------------------------------------------------------------------*
*& Report  ZRD_ZPMT0072_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zpmt0072_exit.


FORM f_exit_zpmt0072_0001 USING p_registro_manter TYPE any.


  DATA: wl_zpmt0072 TYPE zpmt0072.

  CLEAR: wl_zpmt0072.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0072.

  IF wl_zpmt0072-us_criacao IS INITIAL.
    wl_zpmt0072-dt_criacao      = sy-datum.
    wl_zpmt0072-hr_criacao      = sy-uzeit.
    wl_zpmt0072-us_criacao      = sy-uname.
  ENDIF.



  MOVE-CORRESPONDING wl_zpmt0072 TO p_registro_manter.

ENDFORM.

FORM f_exit_zpmt0072_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zpmt0072 TYPE zpmt0072.

  CLEAR: wl_zpmt0072.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0072.

  IF wl_zpmt0072-safra IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Safra é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zpmt0072-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.


  "Valida cento.
  IF wl_zpmt0072-werks IS NOT INITIAL.
    SELECT SINGLE * FROM j_1bbranch INTO @DATA(ws_j_1bbranch) WHERE branch EQ @wl_zpmt0072-werks.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Centro é invalido!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0072-ano_ini IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Ano inicio é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zpmt0072-mes_ini IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Mes inicio é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zpmt0072-ano_fim IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Ano fim é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zpmt0072-mes_fim IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Mes fim é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zpmt0072-mes_ini IS NOT INITIAL.
    IF wl_zpmt0072-mes_ini > 12 OR wl_zpmt0072-mes_ini < 1.
      p_error = abap_true.
      MESSAGE 'Mes inicial é invalido!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0072-mes_fim IS NOT INITIAL.
    IF wl_zpmt0072-mes_fim > 12 OR wl_zpmt0072-mes_fim < 1.
      p_error = abap_true.
      MESSAGE 'Mes final é invalido!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.



  MOVE-CORRESPONDING wl_zpmt0072 TO p_registro_manter.


  CLEAR: p_error, ws_j_1bbranch.

ENDFORM.


FORM f_exit_zpmt0072_0005 CHANGING p_saida TYPE any.

  DATA: wl_zpmt0072 TYPE zpmt0072.

  CLEAR: wl_zpmt0072.

  MOVE-CORRESPONDING p_saida TO wl_zpmt0072.

  IF wl_zpmt0072-us_criacao IS INITIAL.
    wl_zpmt0072-dt_criacao      = sy-datum.
    wl_zpmt0072-hr_criacao      = sy-uzeit.
    wl_zpmt0072-us_criacao      = sy-uname.
  ELSE.
    wl_zpmt0072-dt_modif      = sy-datum.
    wl_zpmt0072-hr_modif      = sy-uzeit.
    wl_zpmt0072-us_modif      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0072 TO p_saida.


ENDFORM.

FORM f_exit_zpmt0072_0004 USING p_registro_manter TYPE any.

*
*  DATA: wl_zpmt0072 TYPE zpmt0072.
*
*  CLEAR: wl_zpmt0072.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0072.
*
*  IF wl_zpmt0072-us_criacao IS INITIAL.
*    wl_zpmt0072-dt_criacao      = sy-datum.
*    wl_zpmt0072-hr_criacao      = sy-uzeit.
*    wl_zpmt0072-us_criacao      = sy-uname.
*  ELSE.
*    wl_zpmt0072-dt_modif      = sy-datum.
*    wl_zpmt0072-hr_modif      = sy-uzeit.
*    wl_zpmt0072-us_modif      = sy-uname.
*  ENDIF.
*
*
*
*  MOVE-CORRESPONDING wl_zpmt0072 TO p_registro_manter.

ENDFORM.

FORM  f_exit_zpmt0072_0009 TABLES pt_excl_toolbar
                           USING p_db_tab.
*
*  TYPES: BEGIN OF ty_excl_toolbar,
*           code TYPE ui_func.
*  TYPES: END OF ty_excl_toolbar.
*
*  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
*        wa_excl_toolbar TYPE ty_excl_toolbar.
*
*  FREE: it_excl_toolbar.
*
*
*  wa_excl_toolbar-code = 'Modificar'.
*  APPEND wa_excl_toolbar  TO it_excl_toolbar.
*
*  pt_excl_toolbar[] = it_excl_toolbar[].
ENDFORM.
