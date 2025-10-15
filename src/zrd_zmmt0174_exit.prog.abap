*&---------------------------------------------------------------------*
*& Report  ZRD_zpmt0074_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zmmt0174_exit.


FORM f_exit_zmmt0174_0001 USING p_registro_manter TYPE any.


  DATA: wl_zmmt0174 TYPE zmmt0174.

  CLEAR: wl_zmmt0174.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0174.

  IF wl_zmmt0174-us_criacao IS INITIAL.
    wl_zmmt0174-dt_criacao      = sy-datum.
    wl_zmmt0174-hr_criacao      = sy-uzeit.
    wl_zmmt0174-us_criacao      = sy-uname.
  ENDIF.

  MOVE-CORRESPONDING wl_zmmt0174 TO p_registro_manter.

ENDFORM.

FORM f_exit_zmmt0174_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zmmt0174 TYPE zmmt0174.

  CLEAR: wl_zmmt0174.

  MOVE-CORRESPONDING p_registro_manter TO wl_zmmt0174.

  IF wl_zmmt0174-grup_comp_sap IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Grupo de compradores SAP é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

*  IF wl_zmmt0174-grup_comp_sap IS NOT INITIAL.
*    SELECT SINGLE * FROM T024 INTO @DATA(ws_T024) WHERE bukrs EQ @wl_zmmt0174-grup_comp_sap.
*    IF sy-subrc NE 0.
*      p_error = abap_true.
*      MESSAGE 'Grupo de compradores é invalido!' TYPE 'S'.
*      EXIT.
*    ENDIF.
*  ENDIF.

  IF wl_zmmt0174-grup_comp_coupa IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Grupo de compradores Coupa é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.



  MOVE-CORRESPONDING wl_zmmt0174 TO p_registro_manter.


  CLEAR: p_error .

ENDFORM.


FORM f_exit_zmmt0174_0005 CHANGING p_saida TYPE any.

*  DATA: wl_zpmt0074 TYPE zpmt0074.
*
*  CLEAR: wl_zpmt0074.
*
*  MOVE-CORRESPONDING p_saida TO wl_zpmt0074.
*
*  IF wl_zpmt0074-us_criacao IS INITIAL.
*    wl_zpmt0074-dt_criacao      = sy-datum.
*    wl_zpmt0074-hr_criacao      = sy-uzeit.
*    wl_zpmt0074-us_criacao      = sy-uname.
*  ELSE.
*    wl_zpmt0074-dt_modif      = sy-datum.
*    wl_zpmt0074-hr_modif      = sy-uzeit.
*    wl_zpmt0074-us_modif      = sy-uname.
*  ENDIF.
*
*  MOVE-CORRESPONDING wl_zpmt0074 TO p_saida.


ENDFORM.

FORM f_exit_zmmt0174_0004 USING p_registro_manter TYPE any.

*
  DATA: wl_zpmt0074 TYPE zpmt0074.
*
  CLEAR: wl_zpmt0074.
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
  MOVE-CORRESPONDING wl_zpmt0074 TO p_registro_manter.

ENDFORM.

FORM  f_exit_zmmt0174_0009 TABLES pt_excl_toolbar
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
