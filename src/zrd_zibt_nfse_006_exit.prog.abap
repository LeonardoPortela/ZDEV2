*&---------------------------------------------------------------------*
*& Include ZRD_ZIBT_NFSE_006_EXIT
*&---------------------------------------------------------------------*

REPORT zrd_zibt_nfse_006_exit.


FORM f_exit_zibt_nfse_006_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zibt_nfse_006 TYPE zibt_nfse_006.
  MOVE-CORRESPONDING p_registro_manter TO wa_zibt_nfse_006.
  wa_zibt_nfse_006-usuario = sy-uname.
  wa_zibt_nfse_006-data = sy-datum.
  MOVE-CORRESPONDING wa_zibt_nfse_006 TO p_registro_manter.

ENDFORM.

FORM f_exit_zibt_nfse_006_0004 CHANGING p_saida TYPE any.

  DATA: wl_zibt_nfse_006_out TYPE zibt_nfse_006_out.

  CLEAR: wl_zibt_nfse_006_out.

  MOVE-CORRESPONDING p_saida TO wl_zibt_nfse_006_out.

  SELECT SINGLE bukrs, butxt
    FROM t001
    INTO @DATA(lwa_t001)
    WHERE bukrs = @wl_zibt_nfse_006_out-bukrs.
  IF sy-subrc EQ 0.
    wl_zibt_nfse_006_out-nome_empresa  = lwa_t001-butxt.
  ENDIF.

  SELECT SINGLE name1
    FROM t001w
    INTO @DATA(lv_name1)
    WHERE werks = @wl_zibt_nfse_006_out-werks.
  IF sy-subrc EQ 0.
    wl_zibt_nfse_006_out-nome_centro  = lv_name1.
  ENDIF.


  MOVE-CORRESPONDING wl_zibt_nfse_006_out TO p_saida.

ENDFORM.



FORM f_exit_zibt_nfse_006_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zibt_nfse_006 TYPE zibt_nfse_006.
  CLEAR:  wa_zibt_nfse_006.

  MOVE-CORRESPONDING p_registro_manter TO wa_zibt_nfse_006.

  CLEAR: p_error.
  IF p_error IS INITIAL.
    IF wa_zibt_nfse_006-bukrs IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo empresa obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zibt_nfse_006-werks IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo centro obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.



FORM f_exit_zibt_nfse_006_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.

FORM f_exit_zibt_nfse_006_0007 TABLES p_table.

ENDFORM.
