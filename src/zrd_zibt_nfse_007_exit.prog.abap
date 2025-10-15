*&---------------------------------------------------------------------*
*& Include ZRD_ZIBT_NFSE_006_EXIT
*&---------------------------------------------------------------------*

REPORT zrd_zibt_nfse_007_exit.


FORM f_exit_zibt_nfse_007_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zibt_nfse_007 TYPE zibt_nfse_007.
  MOVE-CORRESPONDING p_registro_manter TO wa_zibt_nfse_007.
  wa_zibt_nfse_007-usuario = sy-uname.
  wa_zibt_nfse_007-data = sy-datum.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_zibt_nfse_007-lifnr
    IMPORTING
      output = wa_zibt_nfse_007-lifnr.

  MOVE-CORRESPONDING wa_zibt_nfse_007 TO p_registro_manter.

ENDFORM.

FORM f_exit_zibt_nfse_007_0004 CHANGING p_saida TYPE any.

  DATA: wl_zibt_nfse_007_out TYPE zibt_nfse_007_out.

  CLEAR: wl_zibt_nfse_007_out.

  MOVE-CORRESPONDING p_saida TO wl_zibt_nfse_007_out.

  SELECT SINGLE bukrs, butxt
    FROM t001
    INTO @DATA(lwa_t001)
    WHERE bukrs = @wl_zibt_nfse_007_out-bukrs.
  IF sy-subrc EQ 0.
    wl_zibt_nfse_007_out-nome_empresa  = lwa_t001-butxt.
  ENDIF.

  SELECT SINGLE name1
    FROM t001w
    INTO @DATA(lv_name1)
    WHERE werks = @wl_zibt_nfse_007_out-werks.
  IF sy-subrc EQ 0.
    wl_zibt_nfse_007_out-nome_centro  = lv_name1.
  ENDIF.

  SELECT SINGLE name1
  FROM lfa1
  INTO @DATA(lv_name1for)
  WHERE lifnr = @wl_zibt_nfse_007_out-lifnr.
  IF sy-subrc EQ 0.
    wl_zibt_nfse_007_out-nome_for  = lv_name1for.
  ENDIF.


  MOVE-CORRESPONDING wl_zibt_nfse_007_out TO p_saida.

ENDFORM.



FORM f_exit_zibt_nfse_007_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zibt_nfse_007 TYPE zibt_nfse_007.
  CLEAR:  wa_zibt_nfse_007.

  MOVE-CORRESPONDING p_registro_manter TO wa_zibt_nfse_007.

  CLEAR: p_error.
  IF p_error IS INITIAL.
    IF wa_zibt_nfse_007-bukrs IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo empresa obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zibt_nfse_007-werks IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo centro obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zibt_nfse_007-lifnr IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo fornecedor obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.


  IF p_error IS INITIAL.
    SELECT COUNT(*)
      FROM zibt_nfse_006
      WHERE bukrs = wa_zibt_nfse_007-bukrs
         AND werks = wa_zibt_nfse_007-werks.
    IF sy-subrc <> 0.
      p_error = abap_true.
      MESSAGE 'Empresa e filial não cadastrado em ZNFS001!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.



FORM f_exit_zibt_nfse_007_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.

FORM f_exit_zibt_nfse_007_0007 TABLES p_table.

ENDFORM.
