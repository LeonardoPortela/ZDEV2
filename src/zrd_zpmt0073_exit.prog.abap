*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0073_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zpmt0073_exit.


FORM f_exit_zpmt0073_0001 USING p_registro_manter TYPE any.


  DATA: wl_zpmt0073 TYPE zpmt0073.

  CLEAR: wl_zpmt0073.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0073.

  IF wl_zpmt0073-us_criacao IS INITIAL.
    wl_zpmt0073-dt_criacao      = sy-datum.
    wl_zpmt0073-hr_criacao      = sy-uzeit.
    wl_zpmt0073-us_criacao      = sy-uname.
  ENDIF.



  MOVE-CORRESPONDING wl_zpmt0073 TO p_registro_manter.

ENDFORM.

FORM f_exit_zpmt0073_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zpmt0073 TYPE zpmt0073.

  CLEAR: wl_zpmt0073.

  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0073.

  IF wl_zpmt0073-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Empresa é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zpmt0073-bukrs IS NOT INITIAL.
    SELECT SINGLE * FROM t001 INTO @DATA(ws_t001) WHERE bukrs EQ @wl_zpmt0073-bukrs.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Empresa é invalido!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0073-werks IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Centro é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.


  "Valida cento.
  IF wl_zpmt0073-werks IS NOT INITIAL.
    SELECT SINGLE * FROM j_1bbranch INTO @DATA(ws_j_1bbranch) WHERE branch EQ @wl_zpmt0073-werks and bukrs eq @wl_zpmt0073-bukrs.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Centro é invalido!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0073-auart IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Tipo de ordem é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  "Valida tipo de ordem
  IF wl_zpmt0073-auart IS NOT INITIAL.
    SELECT SINGLE * FROM t003o INTO @DATA(ws_t003o)
     WHERE auart EQ @wl_zpmt0073-auart
       AND autyp EQ '30'.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Tipo de ordem é invalido!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  IF wl_zpmt0073-qmart IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Tipo de nota é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  "Valida tipo de ordem
  IF wl_zpmt0073-qmart IS NOT INITIAL.
    SELECT SINGLE * FROM tq80 INTO @DATA(ws_tq80)
     WHERE qmart EQ @wl_zpmt0073-qmart.
    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Tipo de nota é invalido!' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.


  MOVE-CORRESPONDING wl_zpmt0073 TO p_registro_manter.


  CLEAR: p_error, ws_j_1bbranch, ws_t003o, ws_tq80.

ENDFORM.


FORM f_exit_zpmt0073_0005 CHANGING p_saida TYPE any.

  DATA: wl_zpmt0073 TYPE zpmt0073.

  CLEAR: wl_zpmt0073.

  MOVE-CORRESPONDING p_saida TO wl_zpmt0073.

  IF wl_zpmt0073-us_criacao IS INITIAL.
    wl_zpmt0073-dt_criacao      = sy-datum.
    wl_zpmt0073-hr_criacao      = sy-uzeit.
    wl_zpmt0073-us_criacao      = sy-uname.
  ELSE.
    wl_zpmt0073-dt_modif      = sy-datum.
    wl_zpmt0073-hr_modif      = sy-uzeit.
    wl_zpmt0073-us_modif      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zpmt0073 TO p_saida.


ENDFORM.

FORM f_exit_zpmt0073_0004 USING p_registro_manter TYPE any.

**
*  DATA: wl_zpmt0073 TYPE zpmt0073.
**
*  CLEAR: wl_zpmt0073.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zpmt0073.
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
*  MOVE-CORRESPONDING wl_zpmt0073 TO p_registro_manter.

ENDFORM.

FORM  f_exit_zpmt0073_0009 TABLES pt_excl_toolbar
                           USING p_db_tab.
*
  TYPES: BEGIN OF ty_excl_toolbar,
           code TYPE ui_func.
  TYPES: END OF ty_excl_toolbar.

  DATA: it_excl_toolbar TYPE TABLE OF ty_excl_toolbar,
        wa_excl_toolbar TYPE ty_excl_toolbar.

  FREE: it_excl_toolbar.


  wa_excl_toolbar-code = 'Modificar'.
  APPEND wa_excl_toolbar  TO it_excl_toolbar.

  pt_excl_toolbar[] = it_excl_toolbar[].
ENDFORM.
