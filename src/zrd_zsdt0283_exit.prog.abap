*&---------------------------------------------------------------------*
*& Report  ZRD_ZSDT0283_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zsdt0283_exit.

FORM f_exit_zsdt0283_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA wl_zsdt0283 TYPE zsdt0283.

  DATA: lt_dd07v TYPE TABLE OF dd07v .

  CLEAR wl_zsdt0283.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0283.

  IF wl_zsdt0283-finalidade IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Favor informar campo Finalidade!' TYPE 'E'.
    EXIT.
  ELSE.
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZFIN_EXPORT_D'
      TABLES
        values_tab      = lt_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.
    IF sy-subrc = 0.
      READ TABLE lt_dd07v INTO DATA(lwa_dd07v)
        WITH KEY domvalue_l = wl_zsdt0283-finalidade.
      IF sy-subrc IS NOT INITIAL.
        p_error = abap_true.
        MESSAGE 'Favor informar um valor valido no campo finalidade' TYPE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wl_zsdt0283-retorno_nf IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Favor informar o campo Retorno Individual por NF' TYPE 'E'.
    EXIT.
  ELSE.
    CLEAR : lt_dd07v.
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZSDT_RETORNO_NF'
      TABLES
        values_tab      = lt_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.
    IF sy-subrc = 0.
      READ TABLE lt_dd07v INTO lwa_dd07v
        WITH KEY domvalue_l = wl_zsdt0283-retorno_nf.
      IF sy-subrc IS NOT INITIAL.
        p_error = abap_true.
        MESSAGE 'Favor informar um valor valido no campo Retorno Individual por NF' TYPE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wl_zsdt0283-transf_saldo IS INITIAL.

    IF wl_zsdt0283-finalidade EQ 'R' OR wl_zsdt0283-finalidade EQ 'S'.
"Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson Oenning
*      p_error = abap_true.
*      MESSAGE 'Favor informar o Código Operação Transferencia de Saldo !' TYPE 'E'.
*      EXIT.
    ENDIF.

  ENDIF.

  IF wl_zsdt0283-operacao_b IS NOT INITIAL.

    SELECT SINGLE * FROM zfiwrt0001  INTO @DATA(wa_zfiwrt0001)
      WHERE operacao EQ @wl_zsdt0283-operacao_b.

    IF sy-subrc NE 0.
      p_error = abap_true.
      MESSAGE 'Código Operação informado não existe!' TYPE 'E'.
      EXIT.
    ENDIF.
  ELSEIF wl_zsdt0283-finalidade EQ 'Q' OR wl_zsdt0283-finalidade EQ 'S'.
    "Inicio CS2022000880 Ajustes de erros e problemas ZSDT0163 / ZSDT0034 / Anderson Oenning
*    p_error = abap_true.
*    MESSAGE 'Favor informar o Código Operação Baixa de Saldo !' TYPE 'E'.
*    EXIT.
  ENDIF.

  IF wl_zsdt0283-prazo IS NOT INITIAL.
    IF wl_zsdt0283-prazo <> 'D' AND wl_zsdt0283-prazo <> 'F'.
      p_error = abap_true.
      MESSAGE 'Código informado do campo prazo não existe.' TYPE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    p_error = abap_true.
    MESSAGE 'Preencher o campo prazo.' TYPE 'E'.
    EXIT.
  ENDIF.

  IF wl_zsdt0283-bukrs IS NOT INITIAL.
    SELECT SINGLE * FROM t001 INTO @DATA(wa_t001)
      WHERE bukrs = @wl_zsdt0283-bukrs.
    IF sy-subrc IS NOT INITIAL.
      p_error = abap_true.
      MESSAGE 'Empresa informada não existe.' TYPE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    p_error = abap_true.
    MESSAGE 'Preencher o campo empresa.' TYPE 'E'.
    EXIT.
  ENDIF.

  IF wl_zsdt0283-dias CA sy-abcde.
    p_error = abap_true.
    MESSAGE 'Campo dias não pode possuir letras.' TYPE 'E'.
    EXIT.
  ENDIF.

  IF wl_zsdt0283-matkl IS NOT INITIAL.
    SELECT SINGLE * FROM t023 INTO @DATA(wa_t023)
      WHERE matkl = @wl_zsdt0283-matkl.
    IF sy-subrc IS NOT INITIAL.
      p_error = abap_true.
      MESSAGE 'Grupo de mercadorias não existe.' TYPE 'E'.
      EXIT.
    ENDIF.
  ELSE.
    p_error = abap_true.
    MESSAGE 'Preencher grupo de mercadorias.' TYPE 'E'.
    EXIT.
  ENDIF.
ENDFORM.

FORM f_exit_zsdt0283_0004 CHANGING p_saida TYPE any.

  DATA wl_zsdt0283_out TYPE zsdt0283_out.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zsdt0283_out, gv_domvalue_l.

  MOVE-CORRESPONDING p_saida TO wl_zsdt0283_out.

  IF wl_zsdt0283_out-finalidade IS NOT INITIAL.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'ZFIN_EXPORT_D'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zsdt0283_out-finalidade.
    READ TABLE t_dd07v INTO s_dd07v WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      wl_zsdt0283_out-desc_finalidade = s_dd07v-ddtext.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zsdt0283_out TO p_saida.

ENDFORM.


FORM f_exit_zsdt0283_0005 CHANGING p_registro_manter TYPE any.

  DATA wl_zsdt0283 TYPE zsdt0283.

  CLEAR wl_zsdt0283.

  MOVE-CORRESPONDING p_registro_manter TO wl_zsdt0283.

  IF wl_zsdt0283-finalidade EQ 'Q'.
    LOOP AT SCREEN.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-TRANSF_SALDO'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wl_zsdt0283-finalidade EQ 'R'.
    LOOP AT SCREEN.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-OPERACAO_B'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
