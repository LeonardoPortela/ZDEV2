*&---------------------------------------------------------------------*
*& Report  ZRD_ZFIWRT0027_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zfiwrt0027_exit.



FORM f_exit_zfiwrt0027_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfiwrt0027 TYPE zfiwrt0027.

  CLEAR: wl_zfiwrt0027.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfiwrt0027.

  IF wl_zfiwrt0027-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
    EXIT.
  ELSE.
    SELECT SINGLE * FROM t001 INTO @DATA(_wl_t001) WHERE bukrs EQ @wl_zfiwrt0027-bukrs.
    IF sy-subrc EQ 0.
      wl_zfiwrt0027-butxt = _wl_t001-butxt.
    ELSE.
      p_error = abap_true.
      wl_zfiwrt0027-butxt = ' '.
      MESSAGE 'Empresa não encontrada' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  "CS2022001079 Retirar obrigatoriedade do fornecedor ZNFW0017 / Anderson Oenning
*  IF wl_zfiwrt0027-lifnr IS INITIAL AND wl_zfiwrt0027-taxcode IS NOT INITIAL.
*    p_error = abap_true.
*    MESSAGE 'Favor informar o Cod Fornecedor!' TYPE 'S'.
*    EXIT.
*
*  ENDIF.
  "CS2022001079 Retirar obrigatoriedade do fornecedor ZNFW0017 / Anderson Oenning
  IF  wl_zfiwrt0027-lifnr IS NOT INITIAL.

    SELECT SINGLE * FROM lfa1 INTO @DATA(_wl_lfa1) WHERE lifnr EQ  @wl_zfiwrt0027-lifnr.
    IF sy-subrc EQ 0.
      wl_zfiwrt0027-name1 = _wl_lfa1-name1.
    ELSE.
      p_error = abap_true.
      wl_zfiwrt0027-butxt =  ' '.
      MESSAGE 'Fornecedor não encontrado' TYPE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zfiwrt0027 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zfiwrt0027_0003 CHANGING p_saida TYPE any.

  DATA: wl_zfiwrt0027_out TYPE zfiwrt0027_out.

  CLEAR: wl_zfiwrt0027_out.

  MOVE-CORRESPONDING p_saida TO wl_zfiwrt0027_out.

  MOVE-CORRESPONDING wl_zfiwrt0027_out TO p_saida.


ENDFORM.



FORM f_exit_zfiwrt0027_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zfiwrt0027_out TYPE zfiwrt0027_out.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.

  CLEAR: wl_zfiwrt0027_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfiwrt0027_out.


  SELECT SINGLE * FROM t001 INTO wl_t001 WHERE bukrs EQ  wl_zfiwrt0027_out-bukrs.
  IF sy-subrc EQ 0.
    wl_zfiwrt0027_out-butxt = wl_t001-butxt.
  ELSE.
*    MESSAGE 'Empresa não encontrada' TYPE 'S'.
*      EXIT.
    wl_zfiwrt0027_out-butxt = ' '.
  ENDIF.

  SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ  wl_zfiwrt0027_out-lifnr.
  IF sy-subrc EQ 0.
    wl_zfiwrt0027_out-name1 = wl_lfa1-name1.
  ELSE.
*    MESSAGE 'Fornecedor não encontrado' TYPE 'S'.
*    EXIT.
    wl_zfiwrt0027_out-name1 = ' '.
  ENDIF.



  MOVE-CORRESPONDING wl_zfiwrt0027_out TO p_registro_manter.

ENDFORM.

FORM  f_exit_zfiwrt0027_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZFIWRT0027'.
    APPEND 'Modificar'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
