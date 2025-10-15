*&---------------------------------------------------------------------*
*&  Include           ZSDR0087_FORM
*&---------------------------------------------------------------------*

FORM seleciona_dados.

  SELECT * FROM zparam_cont_fret
    INTO TABLE tg_saida.

ENDFORM.

FORM grava_dados.

  DATA: tipom(02)      TYPE c,
        v_kostl(10)    TYPE c,
        t_question(60) TYPE c,
        ans            TYPE c,
        l_count_table  TYPE i.

  PERFORM verifica_erros.

  IF sy-subrc EQ 0.
    DATA: valida_barra TYPE c.

    IF wg_saida-dco IS NOT INITIAL.
      wg_saida-dco = 'S'.
    ELSE.
      wg_saida-dco = 'N'.
    ENDIF.

    " 05.07.2022 - RAMON - 76636 -->
    IF wg_saida-industrializacao IS NOT INITIAL.
      wg_saida-industrializacao = 'S'.
    ELSE.
      wg_saida-industrializacao = 'N'.
    ENDIF.
    " 05.07.2022 - RAMON - 76636 --<

    wg_saida-tp_producao  = tvv3t-kvgr3.
    wg_saida-auart        = tvakt-auart.
    wg_saida-usnam        = sy-uname.
    wg_saida-data_atual   = sy-datum.
    wg_saida-hora_atual   = sy-uzeit.
    APPEND wg_saida TO tg_zparam.

    MODIFY zparam_cont_fret FROM TABLE tg_zparam.
    MESSAGE text-002 TYPE 'S'.
    FREE tg_zparam.

  ENDIF.

  CLEAR: wg_saida, wg_zparam, t001l-lgort.
  REFRESH: tg_zparam, tg_saida.

  PERFORM seleciona_dados.

  IF g_grid IS NOT INITIAL.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDFORM.

FORM deleta_dados.

  LOOP AT tg_selectedrow INTO wg_selectedrow.

    READ TABLE tg_saida INTO wg_saida INDEX wg_selectedrow-index.
    DELETE zparam_cont_fret FROM wg_saida.

  ENDLOOP.

  CLEAR: wg_saida, wg_selectedrow.

  FREE: tg_saida, tg_zparam.

  PERFORM seleciona_dados.

  IF g_grid IS NOT INITIAL.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.             "   DELETA_DADOS


*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*----------------------------------------------------------------------*
FORM double_click  USING    p_e_row
                            p_e_column.

  READ TABLE tg_saida INTO wg_saida INDEX p_e_row.

  IF wg_saida-dco = 'S'.
    wg_saida-dco = 'X'.
  ELSE.
    wg_saida-dco = ' '.
  ENDIF.
  tvv3t-kvgr3 = wg_saida-tp_producao.
  tvakt-auart = wg_saida-auart.



  IF wg_saida IS INITIAL.
    MESSAGE text-001 TYPE 'I'.
  ELSE.

  ENDIF.

ENDFORM.


FORM f4_busca_unidade.
  DATA: wl_unidade    LIKE LINE OF tg_unidade.

*CARREGA LISTA UNIDADE
  SELECT msehi FROM t006
    INTO TABLE tg_unidade.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'MSEHI'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-NRO_SOL_OV'
      value_org   = 'S'
    TABLES
      value_tab   = tg_unidade.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_ZLSCH
*&---------------------------------------------------------------------*
*      BUSCA FORMA DE PAGAMENTO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*      BUSCA FORMA DE PAGAMENTO
FORM f4_busca_zlsch.
  DATA: wl_zlsch    LIKE LINE OF tg_zlsch.

*CARREGA LISTA UNIDADE
  SELECT zlsch text1 FROM t042z
    INTO TABLE tg_zlsch.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ZLSCH'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-ZLSCH'
      value_org   = 'S'
    TABLES
      value_tab   = tg_zlsch.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_ZTERM
*&---------------------------------------------------------------------*
*       Código de utilização
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_busca_abrvw.
  DATA: wl_abrvw    LIKE LINE OF tg_abrvw.

*CARREGA LISTA UNIDADE
  SELECT abrvw bezei FROM tvlvt
    INTO TABLE tg_abrvw
    WHERE spras EQ 'P'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ABRVW'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-ABRVW'
      value_org   = 'S'
    TABLES
      value_tab   = tg_abrvw.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_TIPO_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_busca_tp_calc.

  DATA: wl_tp_calculo LIKE LINE OF tg_tp_calculo.

*CARREGA LISTA TP_CALCULO
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZSDD008'    " Name of domain
      text       = 'X'    " Descriptions - yes/no
    TABLES
      values_tab = tg_tp_calculo.    " Value table.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'DOMVALUE_L'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-TIPO_CALC'
      value_org   = 'S'
    TABLES
      value_tab   = tg_tp_calculo.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_PRECO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_busca_preco .

  DATA: wl_preco LIKE LINE OF tg_preco.

*CARREGA LISTA Tipo Preço
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZSDD011'    " Name of domain
      text       = 'X'    " Descriptions - yes/no
    TABLES
      values_tab = tg_preco.    " Value table.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'DOMVALUE_L'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-PRECO'
      value_org   = 'S'
    TABLES
      value_tab   = tg_preco.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_C_DECIMAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_busca_c_decimais .

  DATA: wl_c_decimais LIKE LINE OF tg_c_decimais.

*CARREGA LISTA TP_CALCULO
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZSDD010'    " Name of domain
      text       = 'X'    " Descriptions - yes/no
    TABLES
      values_tab = tg_c_decimais.    " Value table.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'DOMVALUE_L'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-C_DECIMAIS'
      value_org   = 'S'
    TABLES
      value_tab   = tg_c_decimais.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_PARAM_ESPEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_busca_param_espec .

  DATA: wl_param_espec LIKE LINE OF tg_param_espec.

*CARREGA LISTA TP_CALCULO
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZSDD013'    " Name of domain
      text       = 'X'    " Descriptions - yes/no
    TABLES
      values_tab = tg_param_espec.    " Value table.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'DOMVALUE_L'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-PARAM_ESPEC'
      value_org   = 'S'
    TABLES
      value_tab   = tg_param_espec.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_SAIDA-STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_busca_saida-status .

  DATA: wl_status LIKE LINE OF tg_status.

*CARREGA LISTA TP_CALCULO
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZSDD009'    " Name of domain
      text       = 'X'    " Descriptions - yes/no
    TABLES
      values_tab = tg_status.    " Value table.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'DOMVALUE_L'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SAIDA-STATUS'
      value_org   = 'S'
    TABLES
      value_tab   = tg_status.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       Valida se a filial pertence à empresa informada.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros.

  IF wg_saida-werks IS NOT INITIAL.

    SELECT SINGLE * FROM t001w
    INTO @DATA(wl_t001w)
    WHERE werks EQ @wg_saida-werks.

    IF wl_t001w-vkorg NE wg_saida-bukrs.
      MESSAGE text-001 TYPE 'I'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_BUSCA_SAIDA-KUNNR
*&---------------------------------------------------------------------*
FORM f4_busca_saida-kunnr .

*  DATA lw_shlp TYPE shlp_descr.
*  DATA lt_values TYPE TABLE OF ddshretval.
*  DATA wl_param_espec LIKE LINE OF tg_param_espec.
*
*  lw_shlp-shlptype = 'SH'.
*  lw_shlp-shlpname = 'ZSHLES_F4_KUNNR_OV'.
*
*  CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
*    EXPORTING
*      shlp          = lw_shlp
*    TABLES
*      return_values = lt_values.
*
*READ TABLE
*
**  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
**    EXPORTING
**      RETFIELD    = 'DOMVALUE_L'
**      DYNPPROG    = SY-REPID
**      DYNPNR      = SY-DYNNR
**      DYNPROFIELD = 'WA_SAIDA-KUNNR'
**      VALUE_ORG   = 'S'
**    TABLES
**      VALUE_TAB   = TG_PARAM_ESPEC.





ENDFORM.
