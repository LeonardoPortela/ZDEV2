*&---------------------------------------------------------------------*
*&  Include  ZRD_ZLEST0230_EXIT
*&---------------------------------------------------------------------*
REPORT zrd_zlest0230_exit.

DATA: t_return  TYPE STANDARD TABLE OF ddshretval.

FORM f_exit_zlest0230_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0230 TYPE zlest0230.

  CLEAR: wl_zlest0230.

  wl_zlest0230-zdt_atual   = sy-datum.
  wl_zlest0230-zhr_atual   = sy-uzeit.
  wl_zlest0230-usnam       = sy-uname.

  MOVE-CORRESPONDING wl_zlest0230 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0230_0002    USING p_registro_manter TYPE any
                           CHANGING p_erro.

  DATA: w_zlest0230   TYPE zlest0230_out,
        t_dd07v       TYPE TABLE OF dd07v,
        s_dd07v       TYPE dd07v,
        gv_domvalue_l TYPE dd07v-domvalue_l.

  MOVE-CORRESPONDING p_registro_manter  TO w_zlest0230.

  FREE: t_dd07v.

*  CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT' CS2023000070 - DEVK9A1RRV
*    EXPORTING
*      input  = w_zlest0230-auart
*    IMPORTING
*      output = w_zlest0230-auart.
*
*  IF w_zlest0230-auart IS NOT INITIAL. CS2023000070 - DEVK9A1RRV
*    SELECT SINGLE *
*             FROM tvak
*             INTO @DATA(w_tvak)
*            WHERE auart = @w_zlest0230-auart.
*    IF sy-subrc <> 0.
*      p_erro = abap_true.
*      MESSAGE s024(sd) WITH 'Tipo de Docto. de Venda incorreto.'
*                       DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.

*  IF w_zlest0230-bsart IS NOT INITIAL. CS2023000070 - DEVK9A1RRV
*    SELECT SINGLE *
*             FROM t161
*             INTO @DATA(w_t161)
*            WHERE bsart = @w_zlest0230-bsart.
*    IF sy-subrc <> 0.
*      p_erro = abap_true.
*      MESSAGE s024(sd) WITH 'Tipo de Docto. de Compras incorreto.'
*                       DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*  ENDIF.

*-CS2023000070-07.07.2023-#117578-JT-inicio
*  IF ( w_zlest0230-auart IS NOT INITIAL   AND CS2023000070 - DEVK9A1RRV
*       w_zlest0230-bsart IS NOT INITIAL )  OR
*     ( w_zlest0230-auart IS     INITIAL   AND
*       w_zlest0230-bsart IS     INITIAL ).
*    p_erro = abap_true.
*    MESSAGE s024(sd) WITH 'Preencher Tipo Doc.Vendas ou '
*                          'Tipo de Docto. de Compras!'
*                     DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*-CS2023000070-07.07.2023-#117578-JT-fim

  IF w_zlest0230-parceiro_lr IS NOT INITIAL.  "*-CS2023000070-07.07.2023-#117578-JT
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zlest0230-parceiro_lr
      IMPORTING
        output = w_zlest0230-parceiro_lr.

    SELECT SINGLE *
             FROM kna1
             INTO @DATA(w_kna1)
            WHERE kunnr = @w_zlest0230-parceiro_lr.
    IF sy-subrc <> 0.
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Parceiro LR incorreto.'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

*-CS2023000070-07.07.2023-#117578-JT-inicio
*  IF w_zlest0230-auart       IS NOT INITIAL AND CS2023000070 - DEVK9A1RRV
*     w_zlest0230-bsart       IS NOT INITIAL.
*    p_erro = abap_true.
*    MESSAGE s024(sd) WITH 'Preencher Tipo Doc.Vendas ou '
*                          'Tipo de Docto. de Compras!'
*                     DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

*  IF w_zlest0230-auart       IS INITIAL AND "CS2023000070 - DEVK9A1RRV
*     w_zlest0230-bsart       IS INITIAL AND
*     w_zlest0230-parceiro_lr IS INITIAL.
*    p_erro = abap_true.
*    MESSAGE s024(sd) WITH 'Preencher Tipo Doc.Vendas, ou '
*                          'Tipo de Docto. de Compras, ou '
*                          'o Parceiro LR!'
*                     DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.

  IF w_zlest0230-parceiro_lr IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Preencher o Parceiro LR!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


*-CS2023000070-07.07.2023-#117578-JT-fim

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZVALIDA'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  gv_domvalue_l = w_zlest0230-valida.

  READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE 'Tipo Validar Regra inválido.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING w_zlest0230 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0230_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0230    TYPE zlest0230.

  CLEAR: wl_zlest0230.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0230.

*  CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
*    EXPORTING
*      input  = wl_zlest0230-auart
*    IMPORTING
*      output = wl_zlest0230-auart.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0230-parceiro_lr
    IMPORTING
      output = wl_zlest0230-parceiro_lr.

  wl_zlest0230-zdt_atual   = sy-datum.
  wl_zlest0230-zhr_atual   = sy-uzeit.
  wl_zlest0230-usnam       = sy-uname.

  MOVE-CORRESPONDING wl_zlest0230 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0230_0004 CHANGING p_saida TYPE any.

  DATA: wl_zlest0230  TYPE zlest0230_out,
        t_dd07v       TYPE TABLE OF dd07v,
        s_dd07v       TYPE dd07v,
        gv_domvalue_l TYPE dd07v-domvalue_l.

  CLEAR: wl_zlest0230.

  MOVE-CORRESPONDING p_saida  TO wl_zlest0230.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZVALIDA'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  gv_domvalue_l = wl_zlest0230-valida.

  READ TABLE t_dd07v INTO s_dd07v  WITH KEY domvalue_l = gv_domvalue_l.
  IF sy-subrc = 0.
    wl_zlest0230-descr_valida = s_dd07v-ddtext.
  ENDIF.

  MOVE-CORRESPONDING wl_zlest0230 TO p_saida.

ENDFORM.

FORM f_exit_zlest0230_0005 CHANGING p_registro_manter TYPE any.

  DATA: wl_zlest0230    TYPE zlest0230.

  CLEAR: wl_zlest0230.

  MOVE-CORRESPONDING p_registro_manter  TO wl_zlest0230.

*  CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT' CS2023000070 - DEVK9A1RRV
*    EXPORTING
*      input  = wl_zlest0230-auart
*    IMPORTING
*      output = wl_zlest0230-auart.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zlest0230-parceiro_lr
    IMPORTING
      output = wl_zlest0230-parceiro_lr.

  MOVE-CORRESPONDING wl_zlest0230 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0230_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

ENDFORM.

FORM f_exit_zlest0230_0008 CHANGING p_col_pos
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

*  IF p_ref_tabname = 'ZLEST0230_OUT' AND "CS2023000070 - DEVK9A1RRV
*     p_field       = 'AUART'.
*    p_scrtext_l = 'Tipo Doc. Vendas'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  ENDIF.

*  IF p_ref_tabname = 'ZLEST0230_OUT' AND "CS2023000070 - DEVK9A1RRV
*     p_field       = 'BSART'.
*    p_scrtext_l = 'Tipo Doc. Compras'.
*    p_outputlen = 20.
*    p_f4           = abap_true.
*  ENDIF.

  IF p_ref_tabname = 'ZLEST0230_OUT' AND
     p_field       = 'PARCEIRO_LR'.
    p_scrtext_l = 'Parceiro LR'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0230_OUT' AND
     p_field       = 'VALIDA'.
    p_scrtext_l = 'Valida Regra'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0230_OUT' AND
     p_field       = 'DESCR_VALIDA'.
    p_scrtext_l = 'Descrição'.
    p_outputlen = 30.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0230_OUT' AND
     p_field       = 'USNAM'.
    p_scrtext_l = 'Usuário'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0230_OUT' AND
     p_field       = 'ZDT_ATUAL'.
    p_scrtext_l = 'Data'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

  IF p_ref_tabname = 'ZLEST0230_OUT' AND
     p_field       = 'ZHR_ATUAL'.
    p_scrtext_l = 'Hora'.
    p_outputlen = 20.
    p_f4           = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0230_0013  TABLES p_tables.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      cusobj   = 'ZLEST0230'
      tabfirst = 'X'.

ENDFORM.

FORM f_exit_zlest0230_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_valida USING '<FS_WA_REGISTRO_MANTER>-VALIDA'.
  ENDIF.

ENDFORM.

FORM f4_val_valida USING p_cod TYPE help_info-dynprofld.

  DATA: t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.

*====>  Tabelas internas
  DATA: BEGIN OF t_val OCCURS 0,
          valida TYPE zlest0230-valida,
          descr  TYPE eaml_descr,
        END OF t_val.

  CLEAR t_return.
  DATA: t_mapping TYPE STANDARD TABLE OF dselc.

*====>  Work Area
  DATA: s_return  TYPE ddshretval.
  DATA: s_mapping TYPE dselc.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZVALIDA'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  CHECK t_dd07v[] IS NOT INITIAL.

  LOOP AT t_dd07v INTO s_dd07v.
    t_val-valida     = s_dd07v-domvalue_l.
    t_val-descr      = s_dd07v-ddtext.
    APPEND t_val.
  ENDLOOP.

*  s_mapping-fldname     = 'F0001'.
*  s_mapping-dyfldname   = p_cod.
*  APPEND s_mapping TO t_mapping.
*  CLEAR s_mapping.
*
*  s_mapping-fldname     = 'F0002'.
*  s_mapping-dyfldname   = 'DESCR'.
*  APPEND s_mapping TO t_mapping.
*  CLEAR s_mapping.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VALIDA'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Valida Regra'
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
