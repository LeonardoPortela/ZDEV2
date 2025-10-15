*&---------------------------------------------------------------------*
*& Report  ZRD_zglt0105_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zglt0105_exit.

FORM f_exit_zglt0105_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zglt0105 TYPE zglt0105.

  CLEAR: wl_zglt0105.

*  wl_zglt0105-dt_registro = sy-datum.
*  wl_zglt0105-hr_registro = sy-uzeit.
*  wl_zglt0105-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zglt0105 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt0105_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.
  DATA: wa_zglt0105 TYPE zglt0105.
  CLEAR:  wa_zglt0105.

  MOVE-CORRESPONDING p_registro_manter TO wa_zglt0105.

  CLEAR: p_error.

  IF p_error IS INITIAL.
    IF wa_zglt0105-investidora IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Empresa Investidora Obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF wa_zglt0105-investida IS INITIAL.
      p_error = abap_true.
      MESSAGE 'Campo Empresa Investida Obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.

    DATA: t_dd07v TYPE TABLE OF dd07v,
          s_dd07v TYPE dd07v.
    DATA gv_domvalue_l TYPE dd07v-domvalue_l.


*****  Begin of CS2023000082  #103662 FF   21.02.2023
***    IF wa_zglt0105-tipo_reflexa_pl IS NOT INITIAL.
***
***      CLEAR t_dd07v[].
***      CALL FUNCTION 'GET_DOMAIN_VALUES'
***        EXPORTING
***          domname         = 'ZDTP_REFLEXA_PL'
***        TABLES
***          values_tab      = t_dd07v
***        EXCEPTIONS
***          no_values_found = 1
***          OTHERS          = 2.
***
***      gv_domvalue_l = wa_zglt0105-tipo_reflexa_pl.
***
***      READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
***      IF sy-subrc <> 0.
***        MESSAGE 'Tipo Reflexa inválido.' TYPE 'S' DISPLAY LIKE 'E'.
***        p_error = abap_true.
***        EXIT.
***      ENDIF.
***
***    ENDIF.
  ENDIF.
**  End of FF  21.02.2023


ENDFORM.

FORM f_exit_zglt0105_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zglt0105 TYPE zglt0105.
  MOVE-CORRESPONDING p_registro_manter TO wa_zglt0105.
*  wa_zglt0105-usnam_cad = sy-uname.
*  wa_zglt0105-dt_cad = sy-datum.
*  wa_zglt0105-hr_cad = sy-uzeit.
  MOVE-CORRESPONDING wa_zglt0105 TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt0105_0004 CHANGING p_saida TYPE any.

  DATA: wl_zglt0105_out TYPE zglt0105_out.
  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.
  DATA gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zglt0105_out.


  MOVE-CORRESPONDING p_saida TO wl_zglt0105_out.

  IF  wl_zglt0105_out-saldo IS NOT INITIAL.

    CLEAR t_dd07v[].
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'Z_TP_SALDO'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zglt0105_out-saldo.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      CONCATENATE wl_zglt0105_out-saldo '-' s_dd07v-ddtext INTO wl_zglt0105_out-saldo SEPARATED BY space.

    ENDIF.
  ENDIF.

**  IF wl_zglt0105_out-tipo_reflexa_pl IS NOT INITIAL.
**
**    CLEAR t_dd07v[].
**    CALL FUNCTION 'GET_DOMAIN_VALUES'
**      EXPORTING
**        domname         = 'ZDTP_REFLEXA_PL'
**      TABLES
**        values_tab      = t_dd07v
**      EXCEPTIONS
**        no_values_found = 1
**        OTHERS          = 2.
**
**    gv_domvalue_l = wl_zglt0105_out-tipo_reflexa_pl.
**
**    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
**    IF sy-subrc EQ 0.
**      CONCATENATE wl_zglt0105_out-tipo_reflexa_pl '-' s_dd07v-ddtext INTO wl_zglt0105_out-tipo_reflexa_pl SEPARATED BY space.
**
**    ENDIF.
**  ENDIF.

  IF  wl_zglt0105_out-tp_lancamento IS NOT INITIAL.

    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname         = 'Z_TP_LAN_EQ'
      TABLES
        values_tab      = t_dd07v
      EXCEPTIONS
        no_values_found = 1
        OTHERS          = 2.

    gv_domvalue_l = wl_zglt0105_out-tp_lancamento.

    READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
    IF sy-subrc EQ 0.
      CONCATENATE wl_zglt0105_out-tp_lancamento '-' s_dd07v-ddtext INTO wl_zglt0105_out-tp_lancamento SEPARATED BY space.

    ENDIF.
  ENDIF.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001) WHERE bukrs = @wl_zglt0105_out-investidora.
  IF sy-subrc EQ 0.
    wl_zglt0105_out-nome_investidora  = lwa_t001-butxt.
  ENDIF.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001_in) WHERE bukrs = @wl_zglt0105_out-investida.
  IF sy-subrc EQ 0.
    wl_zglt0105_out-nome_investida  = lwa_t001_in-butxt.
  ENDIF.

  MOVE-CORRESPONDING wl_zglt0105_out TO p_saida.

ENDFORM.

FORM f_exit_zglt0105_0005 CHANGING p_registro_manter TYPE any.

  DATA: wa_zglt0105 TYPE zglt0105.
  DATA: wa_zglt0105_out TYPE zglt0105_out.

  MOVE-CORRESPONDING p_registro_manter TO wa_zglt0105_out.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001) WHERE bukrs = @wa_zglt0105_out-investidora.
  IF sy-subrc EQ 0.
    wa_zglt0105_out-nome_investidora  = lwa_t001-butxt.
  ENDIF.

  SELECT SINGLE bukrs, butxt FROM t001 INTO @DATA(lwa_t001_in) WHERE bukrs = @wa_zglt0105_out-investida.
  IF sy-subrc EQ 0.
    wa_zglt0105_out-nome_investida  = lwa_t001_in-butxt.
  ENDIF.

  MOVE-CORRESPONDING wa_zglt0105_out TO p_registro_manter.

ENDFORM.

FORM f_exit_zglt0105_0006 USING p_registro_manter TYPE any
                       CHANGING p_error.

ENDFORM.

FORM f_exit_zzglt0105_0007 TABLES p_table.

ENDFORM.

*FORM f_exit_zglt0105_0008 CHANGING p_col_pos
*                                   p_ref_tabname
*                                   p_ref_fieldname
*                                   p_tabname
*                                   p_field
*                                   p_scrtext_l
*                                   p_outputlen
*                                   p_edit
*                                   p_sum
*                                   p_emphasize
*                                   p_just
*                                   p_hotspot
*                                   p_f4
*                                   p_check.
*
*  IF p_ref_tabname = 'ZGLT0105_OUT' AND
*    p_field       = 'SALDO'.
*    p_scrtext_l = 'Saldo '.
*    p_outputlen = 20.
*  ENDIF.
*
*  IF p_ref_tabname = 'ZGLT0105_OUT' AND
*     p_field       = 'TP_LANCAMENTO'.
*    p_scrtext_l = 'Tipo Lançamento '.
*    p_outputlen = 40.
*  ENDIF.
*ENDFORM.

FORM f_exit_zglt0105_0008_v2 CHANGING  p_fcat_out TYPE lvc_s_fcat.

  IF p_fcat_out-ref_table = 'ZGLT0105_OUT' AND
   p_fcat_out-ref_field       = 'INVESTIDORA'.
    p_fcat_out-col_pos = 1.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0105_OUT' AND
   p_fcat_out-ref_field      = 'NOME_INVESTIDORA'.
    p_fcat_out-col_pos = 2.
    p_fcat_out-outputlen = 40.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0105_OUT' AND
 p_fcat_out-ref_field       = 'INVESTIDA'.
    p_fcat_out-col_pos = 3.
  ENDIF.

  IF p_fcat_out-ref_table = 'ZGLT0105_OUT' AND
 p_fcat_out-ref_field      = 'NOME_INVESTIDA'.
    p_fcat_out-col_pos = 4.
    p_fcat_out-outputlen = 40.
  ENDIF.


  IF  p_fcat_out-ref_table ='ZGLT0105_OUT' AND
   p_fcat_out-ref_field      = 'SALDO'.
    p_fcat_out-scrtext_l = 'Saldo'.
    p_fcat_out-col_pos = 5.
    p_fcat_out-outputlen = 20.
  ENDIF.

  IF  p_fcat_out-ref_table = 'ZGLT0105_OUT' AND
     p_fcat_out-ref_field      = 'TP_LANCAMENTO'.
    p_fcat_out-scrtext_l = 'Tipo Lançamento'.
    p_fcat_out-outputlen = 40.
    p_fcat_out-col_pos = 6.
  ENDIF.


  IF  p_fcat_out-ref_table =  'ZGLT0105_OUT' AND
    p_fcat_out-ref_field      ='MODELO_ZGL'.
    p_fcat_out-scrtext_l = 'Modelo ZGL014'.
    p_fcat_out-outputlen = 40.
    p_fcat_out-col_pos = 9.
  ENDIF.

ENDFORM.
FORM f_exit_zfit0105_0009  TABLES it_excl_toolbar
                           USING p_db_tab.

ENDFORM.
FORM f_exit_zglt0105_0017 USING p_tipo.

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


  ELSE.
    IF p_tipo = '0002'.
* get the domain values
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'Z_TP_SALDO'   " Give your domain here
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
          dynprofield      = '<FS_WA_REGISTRO_MANTER>-SALDO'
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

    ELSE.
      IF p_tipo = '0003'.
* get the domain values
        CALL FUNCTION 'GET_DOMAIN_VALUES'
          EXPORTING
            domname         = 'Z_TP_LAN_EQ'   " Give your domain here
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
            dynprofield      = '<FS_WA_REGISTRO_MANTER>-TP_LANCAMENTO'
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
    ENDIF.
  ENDIF.
ENDFORM.
