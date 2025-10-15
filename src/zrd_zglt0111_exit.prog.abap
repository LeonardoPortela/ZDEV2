*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0111_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0111_exit.

FORM f_exit_zglt0111_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0111 TYPE zglt0111.

  CLEAR: wl_zglt0111.
  wl_zglt0111-data = sy-datum.
  wl_zglt0111-hora = sy-uzeit.
  wl_zglt0111-usnam = sy-uname.


  MOVE-CORRESPONDING wl_zglt0111 TO p_registro_manter.

ENDFORM.


FORM f_exit_zglt0111_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zglt0111 TYPE zglt0111.
  CLEAR: wl_zglt0111.
  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0111.

  IF wl_zglt0111-per_final IS NOT INITIAL.
    "SELECT SINGLE desc_impost INTO wl_zglt0111-desc_tp_imposto FROM zglt0100 WHERE tp_imposto = wl_zglt0111-tp_imposto.

  ELSE.
    MESSAGE 'Campo Período Final é um campo obrigatório' TYPE 'E'.
    EXIT.
  ENDIF.


  MOVE-CORRESPONDING wl_zglt0111 TO p_registro_manter.


  CLEAR: p_error.

ENDFORM.


FORM f_exit_zglt0111_0003 CHANGING p_saida TYPE any.

  DATA: wl_zglt0111 TYPE zglt0111.
  DATA: gv_data_base TYPE datum.

  CLEAR: wl_zglt0111.

  MOVE-CORRESPONDING p_saida TO wl_zglt0111.

  wl_zglt0111-data = sy-datum.
  wl_zglt0111-hora = sy-uzeit.
  wl_zglt0111-usnam = sy-uname.

  IF wl_zglt0111-investida IS NOT INITIAL AND
   wl_zglt0111-investidora IS NOT INITIAL AND
   wl_zglt0111-per_inicial IS NOT INITIAL.

    gv_data_base = wl_zglt0111-per_inicial.

    SELECT SINGLE moeda_funcional
      INTO @DATA(lv_moeda_funcional)
      FROM zglt0104
      WHERE investidora = @wl_zglt0111-investidora
        AND investida = @wl_zglt0111-investida
        AND inicio_validade <= @gv_data_base
        AND fim_validade >= @gv_data_base.

    IF sy-subrc = 0.

      wl_zglt0111-moeda_funcional = lv_moeda_funcional.

    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zglt0111 TO p_saida.


ENDFORM.

FORM f_exit_zglt0111_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0111_out TYPE zglt0111_out.
  DATA: wl_zglt0111 TYPE zglt0111.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.
  DATA gv_data_base TYPE datum.

  CLEAR: wl_zglt0111_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0111_out.
  CLEAR p_saida.

  IF wl_zglt0111_out-investida IS NOT INITIAL.
    SELECT SINGLE  butxt FROM t001 INTO wl_zglt0111_out-desc_investida WHERE bukrs = wl_zglt0111_out-investida.
  ENDIF.

  IF wl_zglt0111_out-investidora IS NOT INITIAL.
    SELECT SINGLE  butxt FROM t001 INTO wl_zglt0111_out-desc_investidora WHERE bukrs = wl_zglt0111_out-investidora.
  ENDIF.

  IF wl_zglt0111_out-tipo_saldo IS NOT INITIAL.

    CLEAR t_dd07v[].
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'Z_TP_SALDO_EQ'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zglt0111_out-tipo_saldo.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      CONCATENATE wl_zglt0111_out-tipo_saldo '-' s_dd07v-ddtext INTO wl_zglt0111_out-tipo_saldo SEPARATED BY space.

    ENDIF.
  ENDIF.

  IF wl_zglt0111_out-investida IS NOT INITIAL AND
     wl_zglt0111_out-investidora IS NOT INITIAL AND
     wl_zglt0111_out-per_inicial IS NOT INITIAL.

    gv_data_base = wl_zglt0111_out-per_inicial.

    SELECT SINGLE *
      INTO @DATA(ls_zglt0104)
      FROM zglt0104
      WHERE investidora = @wl_zglt0111_out-investidora
        AND investida = @wl_zglt0111_out-investida
        AND inicio_validade <= @gv_data_base
        AND fim_validade >= @gv_data_base.

    IF sy-subrc = 0.

      wl_zglt0111_out-moeda_funcional = ls_zglt0104-moeda_funcional.

    ENDIF.

  ENDIF.

  MOVE-CORRESPONDING wl_zglt0111_out TO p_saida.
ENDFORM.

FORM f_exit_zglt0111_0005 CHANGING p_registro_manter TYPE any.
  DATA: wl_zglt0111_out TYPE zglt0111_out.
  DATA wl_zglt0111  TYPE zglt0111.
  DATA: wl_t001 TYPE  t001,
        wl_lfa1 TYPE  lfa1.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zglt0111_out, wl_t001, wl_lfa1.

  MOVE-CORRESPONDING p_registro_manter TO wl_zglt0111_out.

  IF wl_zglt0111_out-investida IS NOT INITIAL .
    SELECT SINGLE  butxt FROM t001 INTO wl_zglt0111_out-desc_investida WHERE bukrs = wl_zglt0111_out-investida.

  ENDIF.

  IF wl_zglt0111_out-investidora IS NOT INITIAL .
    SELECT SINGLE  butxt FROM t001 INTO wl_zglt0111_out-desc_investidora WHERE bukrs = wl_zglt0111_out-investidora.

  ENDIF.

  IF wl_zglt0111_out-tipo_saldo IS NOT INITIAL.

    CLEAR t_dd07v[].
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'Z_TP_SALDO_EQ'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zglt0111_out-tipo_saldo.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      CONCATENATE wl_zglt0111_out-tipo_saldo '-' s_dd07v-ddtext INTO wl_zglt0111_out-tipo_saldo SEPARATED BY space.

    ENDIF.
  ENDIF.



  MOVE-CORRESPONDING wl_zglt0111_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt0111_0008_v2 CHANGING  p_fcat_out TYPE lvc_s_fcat.


  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'PER_INICIAL'.
    p_fcat_out-col_pos = 1.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'PER_FINAL'.
    p_fcat_out-col_pos = 2.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'INVESTIDORA'.
    p_fcat_out-col_pos = 3.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field      = 'DESC_INVESTIDORA'.
    p_fcat_out-col_pos = 4.
    p_fcat_out-outputlen = 40.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'INVESTIDA'.
    p_fcat_out-col_pos = 5.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field      = 'DESC_INVESTIDA'.
    p_fcat_out-col_pos = 6.
    p_fcat_out-outputlen = 40.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'TIPO_SALDO'.
    p_fcat_out-scrtext_l = 'Tipo de Saldo'.
    p_fcat_out-col_pos = 7.
    p_fcat_out-outputlen = 30.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'VL_PAT_LIQ'.
    p_fcat_out-scrtext_l = 'Valor de Saldo'.
    p_fcat_out-col_pos = 8.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'DATA'.
    p_fcat_out-scrtext_l = 'Data atualização'.
    p_fcat_out-col_pos = 9.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'HORA'.
    p_fcat_out-scrtext_l = 'Hora atualização'.
    p_fcat_out-col_pos = 10.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0111_OUT' AND
    p_fcat_out-ref_field       = 'USNAM'.
    p_fcat_out-scrtext_l = 'Usuário'.
    p_fcat_out-col_pos = 11.
  ENDIF.

ENDFORM.
