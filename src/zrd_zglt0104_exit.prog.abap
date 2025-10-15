*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0104_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0104_exit.

FORM f_exit_zglt0104_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0104 TYPE zglt0104.

  CLEAR: wl_zglt0104.

*  wl_zglt0104-dt_registro = sy-datum.
*  wl_zglt0104-hr_registro = sy-uzeit.
*  wl_zglt0104-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zglt0104 TO p_registro_manter.
ENDFORM.

FORM f_exit_zglt0104_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zglt0104 TYPE zglt0104.
  CLEAR:  wa_zglt0104.

  MOVE-CORRESPONDING p_registro_manter TO wa_zglt0104.

  CLEAR: p_error.
  IF p_error IS INITIAL.
    IF wa_zglt0104-inicio_validade IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Inicio obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0104-investida = '0004' AND
       wa_zglt0104-moeda_funcional <> 'USD'.
      p_error = abap_true.
      MESSAGE 'USD moeda obrigatória para empresa investida' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0104-fim_validade IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Fim obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0104-fim_validade < wa_zglt0104-inicio_validade.
      p_error = abap_true.
      MESSAGE 'Data Fim menor que Data Inicio' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0104-inicio_validade > wa_zglt0104-fim_validade.
      p_error = abap_true.
      MESSAGE 'Data Inicio maior que Data Fim' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0104-investidora IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Empresa Investidora Obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0104-investida IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Empresa Investida Obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0104-moeda_funcional IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Moeda Funcional Obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.


  IF p_error IS INITIAL.
    IF wa_zglt0104-part_perc IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Participação % Obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_exit_zglt0104_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zglt0104 TYPE zglt0104.
  MOVE-CORRESPONDING p_registro_manter TO wa_zglt0104.
*  wa_zglt0104-usnam_cad = sy-uname.
*  wa_zglt0104-dt_cad = sy-datum.
*  wa_zglt0104-hr_cad = sy-uzeit.
  MOVE-CORRESPONDING wa_zglt0104 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt0104_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0104_out TYPE zglt0104_out.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zglt0104_out.

  MOVE-CORRESPONDING p_saida TO wl_zglt0104_out.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001) WHERE bukrs = @wl_zglt0104_out-investidora.
  IF sy-subrc EQ 0.
    wl_zglt0104_out-nome_investidora  = lwa_t001-butxt.
  ENDIF.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001_in) WHERE bukrs = @wl_zglt0104_out-investida.
  IF sy-subrc EQ 0.
    wl_zglt0104_out-nome_investida  = lwa_t001_in-butxt.
  ENDIF.

  CONCATENATE wl_zglt0104_out-inicio_validade+6(2) '/' wl_zglt0104_out-inicio_validade+4(2) '/'  wl_zglt0104_out-inicio_validade+0(4) INTO  wl_zglt0104_out-inicio_validade_form.
  CONCATENATE wl_zglt0104_out-fim_validade+6(2) '/' wl_zglt0104_out-fim_validade+4(2) '/'  wl_zglt0104_out-fim_validade+0(4) INTO  wl_zglt0104_out-fim_validade_form.



  IF wl_zglt0104_out-tipo IS NOT INITIAL.

    CLEAR t_dd07v[].
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'Z_TP_EQ'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zglt0104_out-tipo.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      CONCATENATE wl_zglt0104_out-tipo '-' s_dd07v-ddtext INTO wl_zglt0104_out-tipo SEPARATED BY space.

    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0104_out TO p_saida.

ENDFORM.

FORM f_exit_zglt0104_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zglt0104_out TYPE zglt0104_out.
  MOVE-CORRESPONDING p_registro_manter TO wa_zglt0104_out.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001) WHERE bukrs = @wa_zglt0104_out-investidora.
  IF sy-subrc EQ 0.
    wa_zglt0104_out-nome_investidora  = lwa_t001-butxt.
  ENDIF.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001_in) WHERE bukrs = @wa_zglt0104_out-investida.
  IF sy-subrc EQ 0.
    wa_zglt0104_out-nome_investida  = lwa_t001_in-butxt.
  ENDIF.

  MOVE-CORRESPONDING wa_zglt0104_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zfit0168_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.

FORM f_exit_zfit0168_0007 TABLES p_table.

ENDFORM.

FORM f_exit_zglt0104_0008_v2 CHANGING  p_fcat_out TYPE lvc_s_fcat.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
 p_fcat_out-ref_field       = 'INVESTIDORA'.
    p_fcat_out-col_pos = 1.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
   p_fcat_out-ref_field      = 'NOME_INVESTIDORA'.
    p_fcat_out-col_pos = 2.
    p_fcat_out-outputlen = 40.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
 p_fcat_out-ref_field       = 'INVESTIDA'.
    p_fcat_out-col_pos = 3.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
 p_fcat_out-ref_field      = 'NOME_INVESTIDA'.
    p_fcat_out-col_pos = 4.
    p_fcat_out-outputlen = 40.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
    p_fcat_out-ref_field      = 'PAIS'.
    p_fcat_out-scrtext_l = 'País'.
    p_fcat_out-scrtext_m = 'País'.
    p_fcat_out-scrtext_s = 'País'.
    p_fcat_out-col_pos = 5.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
    p_fcat_out-ref_field      = 'TIPO'.
    p_fcat_out-col_pos = 6.
    p_fcat_out-coltext = 'Tipo'.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
p_fcat_out-ref_field       = 'MOEDA_FUNCIONAL'.
    p_fcat_out-col_pos = 7.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
p_fcat_out-ref_field       = 'PARTICIPACAO_PERC'.
    p_fcat_out-col_pos = 8.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
    p_fcat_out-ref_field       = 'INICIO_VALIDADE_FORM'.
    p_fcat_out-scrtext_l = 'Inicio'.
    p_fcat_out-outputlen = 10.
    p_fcat_out-col_pos = 9.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
     p_fcat_out-ref_field       = 'FIM_VALIDADE_FORM'.
    p_fcat_out-scrtext_l = 'Fim'.
    p_fcat_out-outputlen = 10.
    p_fcat_out-col_pos = 10.
  ENDIF.


  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
     p_fcat_out-ref_field       = 'MOEDA_FUNCIONAL'.
    p_fcat_out-f4availabl = 'X'.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
    p_fcat_out-ref_field       = 'INICIO_VALIDADE'.
    p_fcat_out-no_out = 'X'.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0104_OUT' AND
     p_fcat_out-ref_field       = 'FIM_VALIDADE'.
    p_fcat_out-scrtext_l = 'Fim'.
    p_fcat_out-no_out = 'X'.
  ENDIF.

ENDFORM.
FORM f_exit_zglt0104_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

ENDFORM.

FORM f_exit_zglt0104_0017 USING p_tipo.

  DATA: BEGIN OF s_data,
          domvalue_l TYPE domvalue_l,
          ddtext     TYPE val_text,
        END OF s_data,
        t_data LIKE TABLE OF s_data.
  DATA: t_dd07v TYPE STANDARD TABLE OF dd07v,
        s_dd07v TYPE dd07v.

  REFRESH: t_data.

  IF p_tipo = '0001'.
* Get the domain values
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'Z_MOEDA_FUNC'   " Give your domain here
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

* Prepare the data.
    LOOP AT t_dd07v INTO s_dd07v.
      MOVE-CORRESPONDING s_dd07v TO s_data.
      APPEND s_data TO t_data.
    ENDLOOP.

* F4
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        pvalkey          = ' '
        retfield         = 'DOMVALUE_L'
        dynpprog         = sy-repid
        dynpnr           = sy-dynnr
        dynprofield      = '<FS_WA_REGISTRO_MANTER>-MOEDA_FUNCIONAL'
        callback_program = sy-repid
        value_org        = 'S'
      TABLES
        value_tab        = t_data
      EXCEPTIONS
        parameter_error  = 1
        no_values_found  = 2
        OTHERS           = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      "BREAK-POINT.
    ENDIF.
  ENDIF.


ENDFORM.
