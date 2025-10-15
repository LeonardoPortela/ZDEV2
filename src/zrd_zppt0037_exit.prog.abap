*&---------------------------------------------------------------------*
*&  Include  ZRD_ZPPT0037_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zppt0037_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zppt0037_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0037 TYPE zppt0037.

  CLEAR: wl_zppt0037.

  wl_zppt0037-data        = sy-datum.
  wl_zppt0037-hora        = sy-uzeit.
  wl_zppt0037-usuario     = sy-uname.

  MOVE-CORRESPONDING wl_zppt0037 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0037_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zppt0037    TYPE zppt0037_out,
        t_dd07v       TYPE TABLE OF dd07v,
        s_dd07v       TYPE dd07v,
        l_perc        TYPE numc3,
        gv_domvalue_l TYPE dd07v-domvalue_l.

  MOVE-CORRESPONDING p_registro_manter  TO w_zppt0037.

  FREE: t_dd07v.

  CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
    EXPORTING
      input  = w_zppt0037-werks
    IMPORTING
      output = w_zppt0037-werks.

  SELECT SINGLE *
           FROM t001w
           INTO @DATA(w_t001w)
          WHERE werks = @w_zppt0037-werks.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Filial não Cadastrada!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZPROC_ALGD'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  gv_domvalue_l = w_zppt0037-processo.

  READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE 'Tipo Processo Inválido!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT *
    FROM zppt0037
    INTO TABLE @DATA(t_0037)
   WHERE werks   <> @w_zppt0037-werks
     AND processo = @w_zppt0037-processo.

  FREE: l_perc.

  LOOP AT t_0037 INTO DATA(w_0037).
    l_perc = l_perc + w_0037-perc_jobs.
  ENDLOOP.

  l_perc = l_perc + w_zppt0037-perc_jobs.

  IF l_perc > 100.
    p_erro = abap_true.
    MESSAGE 'Percentual não pode exceder 100% !' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* IF w_zppt0037-perc_jobs IS INITIAL OR
  IF w_zppt0037-perc_jobs > 100.
    p_erro = abap_true.
    MESSAGE 'Percentual de Jobs Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zppt0037-periodo_ini IS INITIAL OR
     w_zppt0037-periodo_fim IS INITIAL OR
     w_zppt0037-periodo_ini > w_zppt0037-periodo_fim.
    p_erro = abap_true.
    MESSAGE 'Período Informado está Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zppt0037 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0037_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0037    TYPE zppt0037.

  CLEAR: wl_zppt0037.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zppt0037.

  CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
    EXPORTING
      input  = wl_zppt0037-werks
    IMPORTING
      output = wl_zppt0037-werks.

  wl_zppt0037-data                 = sy-datum.
  wl_zppt0037-hora                 = sy-uzeit.
  wl_zppt0037-usuario              = sy-uname.
  wl_zppt0037-reinicia_job         = abap_true.

  UPDATE zppt0037 SET reinicia_job = abap_true.

  MOVE-CORRESPONDING wl_zppt0037 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0037_0004 CHANGING p_saida TYPE any.

  DATA: wl_zppt0037   TYPE zppt0037_out,
        t_dd07v       TYPE TABLE OF dd07v,
        s_dd07v       TYPE dd07v,
        gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zppt0037.

  MOVE-CORRESPONDING p_saida  TO wl_zppt0037.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZPROC_ALGD'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  gv_domvalue_l = wl_zppt0037-processo.

  READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
  IF sy-subrc = 0.
    wl_zppt0037-descr_processo = s_dd07v-ddtext.
  ENDIF.

  MOVE-CORRESPONDING wl_zppt0037 TO p_saida.

ENDFORM.

FORM f_exit_zppt0037_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0037    TYPE zppt0037.

  CLEAR: wl_zppt0037.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zppt0037.

  CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
    EXPORTING
      input  = wl_zppt0037-werks
    IMPORTING
      output = wl_zppt0037-werks.

  MOVE-CORRESPONDING wl_zppt0037 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0037_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

  UPDATE zppt0037 SET reinicia_job = abap_true.

ENDFORM.

FORM f_exit_zppt0037_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'WERKS'.
    p_scrtext_l = 'Filial'.
    p_outputlen = 10.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'PROCESSO'.
    p_scrtext_l = 'Processo'.
    p_outputlen = 10.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'DESCR_PROCESSO'.
    p_scrtext_l = 'Descrição'.
    p_outputlen = 40.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'PERC_JOBS'.
    p_scrtext_l = 'Percentual JOBS Execução'.
    p_outputlen = 25.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'PERIODO_INI'.
    p_scrtext_l = 'Período Inicial'.
    p_outputlen = 14.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'PERIODO_FIM'.
    p_scrtext_l = 'Período Final'.
    p_outputlen = 14.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'USUARIO'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'DATA'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0037_OUT' AND
     p_field       = 'HORA'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zppt0037_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZPPT0037'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zppt0037_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_valida USING '<FS_WA_REGISTRO_MANTER>-PROCESSO'.
  ENDIF.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.

*====>  Tabelas internas
  DATA: BEGIN OF t_val OCCURS 0,
          processo TYPE zppt0037-processo,
          descr    TYPE eaml_descr,
        END OF t_val.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZPROC_ALGD'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  CHECK t_dd07v[] IS NOT INITIAL.

  LOOP AT t_dd07v INTO s_dd07v.
    t_val-processo   = s_dd07v-domvalue_l.
    t_val-descr      = s_dd07v-ddtext.
    APPEND t_val.
  ENDLOOP.

  s_mapping-fldname     = 'F0001'.
  s_mapping-dyfldname   = p_cod.
  APPEND s_mapping TO t_mapping.
  CLEAR s_mapping.
*
  s_mapping-fldname     = 'F0002'.
  s_mapping-dyfldname   = 'DESCR'.
  APPEND s_mapping TO t_mapping.
  CLEAR s_mapping.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZPROC_ALGD'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Processo'
      value_org       = 'S'
    TABLES
      value_tab       = t_val
      return_tab      = t_return
      dynpfld_mapping = t_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
