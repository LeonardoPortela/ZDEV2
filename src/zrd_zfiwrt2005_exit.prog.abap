*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIWRT0027_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfiwrt2005_exit.

FORM f_exit_zfiwrt2005_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfiwrt2005 TYPE zfiwrt2005.

  CLEAR: wl_zfiwrt2005.

*  IF sy-ucomm EQ 'CRIAR'.
    wl_zfiwrt2005-us_criacao    = sy-uname.
    wl_zfiwrt2005-dt_criacao    = sy-datum.
    wl_zfiwrt2005-hr_criacao    = sy-uzeit.

    wl_zfiwrt2005-us_modificacao   = sy-uname.
    wl_zfiwrt2005-dt_modificacao    = sy-datum.
    wl_zfiwrt2005-hr_modificacao    = sy-uzeit.

*  ELSE.
*    wl_zfiwrt2005-us_modificacao   = sy-uname.
*    wl_zfiwrt2005-dt_modificacao    = sy-datum.
*    wl_zfiwrt2005-hr_modificacao    = sy-uzeit.
*  ENDIF.


  MOVE-CORRESPONDING wl_zfiwrt2005 TO p_registro_manter.

ENDFORM.

FORM f_exit_zfiwrt2005_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfiwrt2005 TYPE zfiwrt2005.

  CLEAR: wl_zfiwrt2005.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfiwrt2005.

  IF wl_zfiwrt2005-usuario IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Usuario é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zfiwrt2005-operacao IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Operação é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF wl_zfiwrt2005-dt_lanc IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Data lançamento é obrigatório!' TYPE 'S'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING wl_zfiwrt2005 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zfiwrt2005_0003 CHANGING p_saida TYPE any.

  DATA: wl_zfiwrt2005_out TYPE zfiwrt2005.

  CLEAR: wl_zfiwrt2005_out.

  MOVE-CORRESPONDING p_saida TO wl_zfiwrt2005_out.

  MOVE-CORRESPONDING wl_zfiwrt2005_out TO p_saida.


ENDFORM.



*FORM f_exit_zfiwrt0027_0005 CHANGING p_registro_manter TYPE any.
*  DATA: wl_zfiwrt0027_out TYPE zfiwrt0027_out.
*  DATA: wl_t001 TYPE  t001,
*        wl_lfa1 TYPE  lfa1.
*
*  CLEAR: wl_zfiwrt0027_out, wl_t001, wl_lfa1.
*
*  MOVE-CORRESPONDING p_registro_manter TO wl_zfiwrt0027_out.
*
*
*  SELECT SINGLE * FROM t001 INTO wl_t001 WHERE bukrs EQ  wl_zfiwrt0027_out-bukrs.
*  IF sy-subrc EQ 0.
*    wl_zfiwrt0027_out-butxt = wl_t001-butxt.
*  ELSE.
**    MESSAGE 'Empresa não encontrada' TYPE 'S'.
**      EXIT.
*    wl_zfiwrt0027_out-butxt = ' '.
*  ENDIF.
*
*  SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ  wl_zfiwrt0027_out-lifnr.
*  IF sy-subrc EQ 0.
*    wl_zfiwrt0027_out-name1 = wl_lfa1-name1.
*  ELSE.
**    MESSAGE 'Fornecedor não encontrado' TYPE 'S'.
**    EXIT.
*    wl_zfiwrt0027_out-name1 = ' '.
*  ENDIF.
*
*
*
*  MOVE-CORRESPONDING wl_zfiwrt0027_out TO p_registro_manter.
*
*ENDFORM.

*FORM  f_exit_zfiwrt0027_0009 TABLES it_excl_toolbar
*                           USING p_db_tab.
*
*  IF p_db_tab = 'ZFIWRT0027'.
*    APPEND 'Modificar'    TO it_excl_toolbar.
*  ENDIF.
*ENDFORM.
