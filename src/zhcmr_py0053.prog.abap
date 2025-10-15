**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*


**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Carolini Santos ( carolini.santos@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| TGG - Refeitorio - API Consulta                                           |*
**/===========================================================================\*


REPORT zhcmr_py0053.

TABLES: pa0465, sscrfields, zhcmt_py_0025.

SELECTION-SCREEN BEGIN OF BLOCK blo_0 WITH FRAME.

SELECTION-SCREEN END OF BLOCK blo_0.

SELECTION-SCREEN BEGIN OF BLOCK blo_1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: o_mes FOR zhcmt_py_0025-mespr NO-EXTENSION NO INTERVALS,
                o_ano FOR zhcmt_py_0025-anopr NO-EXTENSION NO INTERVALS,
                o_cpf FOR pa0465-cpf_nr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK blo_1.

SELECTION-SCREEN BEGIN OF BLOCK blo_2 WITH FRAME TITLE text-002.
PARAMETERS: p_fun TYPE char1  RADIOBUTTON GROUP rb01,
            p_tod TYPE char1  RADIOBUTTON GROUP rb01.
SELECTION-SCREEN END OF BLOCK blo_2.

SELECTION-SCREEN: FUNCTION KEY 1,  "// FC01 - Consulta quantidade acessos
                  FUNCTION KEY 2.  "// FC03 - Envio de ausências

*// Tipos de Dados
TYPES:  BEGIN OF  ty_saida.
          INCLUDE STRUCTURE zhcmt_py_0025.
          TYPES: nome_bukrs  TYPE butxt,
          data_inicio TYPE sy-datum,
          data_fim    TYPE sy-datum,
          nome_werks  TYPE pbtxt,
          status_desc TYPE char20,
          cellcolor   TYPE lvc_t_scol,
        END OF ty_saida.

DATA: gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid,
      git_fieldcat     TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY,
      gwa_layout       TYPE lvc_s_layo.

"// Tabelas
DATA: it_saida     TYPE TABLE OF ty_saida,
      it_pa0465    TYPE TABLE OF pa0465,
      it_zhcmt0004 TYPE TABLE OF zhcmt0004,
      it_pa0001    TYPE TABLE OF pa0001,
      it_0003      TYPE TABLE OF zhcme_py_0003.

DATA: wa_0003 TYPE zhcme_py_0003,
      r_subty TYPE zrsdsselopts.

"// Variaveis
DATA: last_day    TYPE endda,
      first_day   TYPE endda,
      mes_ano     TYPE char8,
      mes         TYPE t247-ktx,
      datastamp   TYPE timestamp,
      empresa_tgg TYPE pa0001-bukrs VALUE '0048',
      p_rubrica   TYPE tvarvc-low.

INITIALIZATION.

  sscrfields =
  VALUE #(
            functxt_01 = |{ icon_next_hierarchy_level }     { text-021 }|
            functxt_02 = |{ icon_previous_hierarchy_level } { text-022 }|
          ).

AT SELECTION-SCREEN.
  DATA(direcao) = COND #(
                          WHEN sy-ucomm EQ 'FC01' THEN 1
                          WHEN sy-ucomm EQ 'FC02' THEN 2
                          WHEN sy-batch EQ abap_true THEN 2
                        ).

  PERFORM fm_executar_api USING direcao.

START-OF-SELECTION.
  CHECK sy-batch IS INITIAL.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  CHECK sy-batch IS INITIAL.
  PERFORM fm_end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona.

  SELECT *
    FROM zhcmt_py_0025
    INTO CORRESPONDING FIELDS OF TABLE it_saida
    WHERE anopr  EQ o_ano-low
      AND mespr  EQ o_mes-low
      AND nr_cpf IN o_cpf.

  SELECT SINGLE low
   FROM tvarvc
    INTO @p_rubrica
   WHERE name EQ 'RUBRICA_ZHCMR_PY0053'.

  SELECT SINGLE ktx
    FROM t247
    INTO @mes
        WHERE spras EQ @sy-langu
        AND   mnr   EQ @o_mes-low.

  SELECT *
    FROM zhcmt0004
    INTO TABLE it_zhcmt0004
    WHERE anopr EQ o_ano-low
      AND mespr EQ o_mes-low.

  CHECK it_saida IS NOT INITIAL.

  SELECT *
    FROM pa0465
    INTO TABLE it_pa0465
    FOR ALL ENTRIES IN it_saida
    WHERE cpf_nr EQ it_saida-nr_cpf.

  CHECK it_pa0465 IS NOT INITIAL.

  SELECT *
    FROM pa0001
    INTO TABLE it_pa0001
    FOR ALL ENTRIES IN it_pa0465
    WHERE pernr EQ it_pa0465-pernr
      AND bukrs EQ empresa_tgg.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .


  DATA: ls_color TYPE lvc_s_scol.

  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<f_saida>).

    READ TABLE it_zhcmt0004 INTO DATA(wa_zhcmt0004) WITH KEY anopr = <f_saida>-anopr mespr = <f_saida>-mespr.
    IF sy-subrc IS INITIAL.
      <f_saida>-data_inicio = wa_zhcmt0004-initm.
      <f_saida>-data_fim    = wa_zhcmt0004-fimtm.
    ENDIF.

    READ TABLE it_pa0465 INTO DATA(wa_pa0465) WITH KEY cpf_nr = <f_saida>-nr_cpf.
    IF sy-subrc IS INITIAL.
      READ TABLE it_pa0001 INTO DATA(wa_pa0001) WITH KEY pernr = wa_pa0465-pernr.
      IF sy-subrc IS INITIAL.

        <f_saida>-bukrs = wa_pa0001-bukrs.
        <f_saida>-werks = wa_pa0001-werks.

        CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
          EXPORTING
            bukrs      = <f_saida>-bukrs
            langu      = sy-langu
          IMPORTING
            bukrs_text = <f_saida>-nome_bukrs.

        CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
          EXPORTING
            werks      = <f_saida>-werks
          IMPORTING
            werks_text = <f_saida>-nome_werks.

      ENDIF.
    ENDIF.

    <f_saida>-status_desc = COND #( WHEN <f_saida>-status EQ abap_false THEN text-023
                                    WHEN <f_saida>-status EQ abap_true  THEN text-024
                                    WHEN <f_saida>-status EQ 'E'        THEN text-025
    ).


    REFRESH: <f_saida>-cellcolor.
    CLEAR: ls_color.

    APPEND VALUE lvc_s_scol(
                              fname = 'STATUS_DESC'
                              color-col = COND #( WHEN <f_saida>-status EQ abap_false THEN cl_gui_resources=>list_col_total
                                                  WHEN <f_saida>-status EQ abap_true  THEN cl_gui_resources=>list_col_positive
                                                  WHEN <f_saida>-status EQ 'E'        THEN cl_gui_resources=>list_col_negative )
                              color-int = 0
                              color-inv = 0
                            ) TO <f_saida>-cellcolor.

  ENDLOOP.

*  DATA: im_date TYPE begda.

  first_day = |{ o_ano-low }{ o_mes-low }01|.

*  CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*    EXPORTING
*      im_date              = im_date
*    IMPORTING
*      ex_last_day_of_month = last_day.

  mes = |{ mes(1) CASE = UPPER }{ mes+1(2) CASE = LOWER }|.
  mes_ano = |{ mes }/{ o_ano-low }|.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.
  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'DESCONTO'.
      PERFORM fm_executar_desconto.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_BUSCA_ACESSO_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM fm_busca_acesso_api .
*  PERFORM fm_indicador USING 'Conexão com API Acesso'.
*
*  DATA: wa_token TYPE ztoken,
*        e_token  TYPE string.
*
*  GET TIME STAMP FIELD datastamp.
*
*  SELECT SINGLE *
*    FROM ztoken
*    INTO wa_token
*    WHERE modulo EQ 'ACESSO_TGG'.
*
*  IF sy-subrc IS NOT INITIAL.
*
*    PERFORM fm_get_token USING e_token.
*
*  ELSE.
*
**   "// Verifica o Vencimento do Token
*    IF sy-datum EQ wa_token-data AND sy-uzeit < wa_token-hora.
*      e_token = |Bearer { wa_token-token }|.
*    ELSE.
*      PERFORM fm_get_token USING e_token.
*    ENDIF.
*
*  ENDIF.

*  PERFORM fm_get_acesso_api USING e_token.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ENVIAR_AUSENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM fm_enviar_ausencia USING p_token.
**  PERFORM fm_indicador USING 'Conexão com API Ausencia'.
*
*  DATA: t_set  TYPE TABLE OF rgsb4,
*        wa_set TYPE rgsb4.
*
*  SELECT pernr
*    FROM pa0001
*    INTO TABLE @DATA(it_pa0001)
*      WHERE bukrs EQ '0048'
*        AND endda >= @sy-datum
*        AND abkrs NE 'BA'.
*
*  IF sy-subrc IS INITIAL.
*
*    SELECT pernr
*      FROM pa0000
*      INTO TABLE @DATA(it_pa0000)
*      FOR ALL ENTRIES IN @it_pa0001
*        WHERE pernr EQ @it_pa0001-pernr
*          AND stat2 EQ '0'
*          AND begda >= @sy-datum.
*
*    IF sy-subrc IS INITIAL.
*
*      SELECT *
*        FROM pa0465
*        INTO TABLE @DATA(it_pa0465)
*        FOR ALL ENTRIES IN @it_pa0000
*          WHERE pernr EQ @it_pa0000-pernr
*            AND endda >= @sy-datum.
*
*    ENDIF.
*
*  ENDIF.
*
*  LOOP AT it_pa0465 INTO DATA(wa_pa0465).
*
*    CALL FUNCTION 'RP_GET_FIRE_DATE'
*      EXPORTING
*        persnr   = wa_pa0465-pernr
*      IMPORTING
*        firedate = wa_0003-_data_inicio.
*
*    wa_0003-_documento = wa_pa0465-cpf_nr.
*    wa_0003-_restricao_requisito = '9999'.
*
*    APPEND wa_0003 TO it_0003.
*
*  ENDLOOP.
*
*  SELECT pernr
*     FROM pa0001
*     INTO TABLE @it_pa0001
*       WHERE bukrs EQ '0048'
*         AND endda >= @sy-datum
*    AND plans NE '99999999'
*         AND abkrs NE 'BA'.
*
*  IF sy-subrc IS INITIAL.
*
*    SELECT *
*      FROM pa0465
*      INTO TABLE @it_pa0465
*      FOR ALL ENTRIES IN @it_pa0001
*        WHERE pernr EQ @it_pa0001-pernr
*          AND endda >= @sy-datum.
*
*
*    SELECT 'I'   AS valsign,
*           'EQ'  AS valoption,
*           valfrom AS low
*      FROM setleaf
*        INTO TABLE @r_subty
*          WHERE setname = 'MAGGI_AUSENCIA_REF_TTG'.
*
*    SELECT *
*      FROM pa2001
*      INTO TABLE @DATA(it_pa2001)
*      FOR ALL ENTRIES IN @it_pa0001
*        WHERE pernr EQ @it_pa0001-pernr
*          AND subty IN @r_subty
*          AND endda >= @sy-datum
*          AND begda <= @sy-datum.
*
*  ENDIF.
*
*  LOOP AT it_pa2001 INTO DATA(wa_pa2001).
*
*    READ TABLE it_pa0465 INTO wa_pa0465 WITH KEY pernr = wa_pa2001-pernr.
*
*    wa_0003 =
*    VALUE #(
*             _documento          = wa_pa0465-cpf_nr
*             _restricao_requisito = wa_pa2001-subty
*             _data_inicio         = wa_pa2001-begda
*             _data_termino        = wa_pa2001-endda
*           ).
*
*    APPEND wa_0003 TO it_0003.
*
*  ENDLOOP.
*
*  IF it_0003 IS INITIAL.
*    MESSAGE 'Não foi encontrado nenhuma informação!' TYPE 'W'.
*    EXIT.
*  ENDIF.
*
*  PERFORM fm_enviar_ausencia_api USING p_token.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .

  DATA: i_filtros TYPE zif_screen_linha_filtro_t,
        vl_text   TYPE sdydo_text_element.

  PERFORM fm_cria_fieldcat.

  vl_text = text-003.

  zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
         i_titulo  = CONV #( vl_text )
         i_filtros = VALUE zif_screen_linha_filtro_t(
                                                        ( parametro = '' valor = '' )
                                                        ( parametro = 'Rubrica para desconto:' valor = p_rubrica )
                                                        ( parametro = 'Mês/Ano Folha:' valor = mes_ano )
                                                    )
       CHANGING
         alv = gob_gui_alv_grid
       ).

  gwa_layout =
  VALUE #(
           sel_mode   = 'A'
           box_fname  = abap_true
           ctab_fname = 'CELLCOLOR'
         ).

  CALL METHOD gob_gui_alv_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = gwa_layout
    CHANGING
      it_outtab                     = it_saida
      it_fieldcatalog               = git_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY,
         lit_fieldcat_log TYPE TABLE OF lvc_t_fcat WITH DEFAULT KEY.

  FREE git_fieldcat.

  git_fieldcat     = VALUE lit_fieldcat_aux(
*        ( fieldname = 'ANOPR'        coltext = text-033 col_opt = abap_true no_zero = abap_false )
*        ( fieldname = 'MESPR'        coltext = text-034 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'DATA_INICIO'  coltext = text-004 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'DATA_FIM'     coltext = text-005 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'COD_RETORNO'  coltext = text-006 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'MSG_RETORNO'  coltext = text-007 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'BUKRS'        coltext = text-008 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'NOME_BUKRS'   coltext = text-009 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'WERKS'        coltext = text-010 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'NOME_WERKS'   coltext = text-011 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'PERNR'        coltext = text-012 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'CNAME'        coltext = text-013 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'NR_CPF'       coltext = text-014 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'PERFIL'       coltext = text-015 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'QTDE_ACESSOS' coltext = text-016 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'USER_NAME'    coltext = text-017 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'DATA'         coltext = text-018 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'HORA'         coltext = text-019 col_opt = abap_true no_zero = abap_false )
        ( fieldname = 'STATUS_DESC'  coltext = text-020 col_opt = abap_true no_zero = abap_false just = 'C' )
       ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_EXECUTAR_DESCONTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_executar_desconto .

  DATA: lt_0015       TYPE TABLE OF p0015,
        w_0015        TYPE p0015,
        ls_bapireturn TYPE bapireturn1,
        vl_info       TYPE prelp-infty VALUE '0015',
        vl_acao       TYPE actio VALUE 'INS'.

  CALL METHOD gob_gui_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lit_rows).

  LOOP AT lit_rows[] INTO DATA(lwa_rows).

    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<f_saida>) INDEX lwa_rows-index.
    CHECK sy-subrc IS INITIAL.

    IF <f_saida>-pernr IS INITIAL.
      MESSAGE text-032 TYPE 'W'.
      EXIT.
    ENDIF.

    IF <f_saida>-status EQ abap_true.
      MESSAGE text-031 TYPE 'W'.
      EXIT.
    ENDIF.

    w_0015 =
    VALUE #(
            infty = vl_info
            pernr = <f_saida>-pernr
            lgart = p_rubrica
            subty = p_rubrica
            waers = 'BRL'
            anzhl = <f_saida>-qtde_acessos
            begda = first_day
           ).

    CALL METHOD zcl_hrst_commons=>hr_infotype_operation
      EXPORTING
        iv_infty  = vl_info   " Infotipo
        iv_actio  = vl_acao   " Operação em infotipos
        iw_pnnnn  = w_0015
        iv_tclas  = 'A'      " Classe de transação para retenção de dados
      CHANGING
        cv_return = ls_bapireturn.    " Parâmetro de retorno

    <f_saida>-status = abap_false.

    <f_saida>-status = COND #( WHEN ls_bapireturn IS INITIAL THEN abap_true ELSE 'E').

    UPDATE zhcmt_py_0025 SET status = <f_saida>-status
    WHERE nr_cpf EQ <f_saida>-nr_cpf
      AND anopr  EQ <f_saida>-anopr
      AND mespr  EQ <f_saida>-mespr.

    CLEAR: w_0015, ls_bapireturn.

  ENDLOOP.

  PERFORM fm_dados_processa.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_INDICADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT  text
*----------------------------------------------------------------------*
FORM fm_indicador USING p_text.

  DATA cont TYPE i.
  DATA cont_porcentagem TYPE i.
  DATA: vl_texto TYPE string.

  vl_texto = p_text.
  cont = 1.

  DO.

    IF cont EQ 10.
      EXIT.
    ENDIF.

    CLEAR vl_texto.
    vl_texto = p_text.

    vl_texto = SWITCH #( cont
    WHEN 0 OR 3 OR 6 THEN |{ vl_texto } >|
    WHEN 1 OR 4 OR 7 THEN |{ vl_texto } >>|
    WHEN 2 OR 5 OR 8 THEN |{ vl_texto } >>>|
    WHEN 9 THEN |Falha na Conexão!|
    ).

    cont_porcentagem = 10.
    cont_porcentagem = cont_porcentagem * cont.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = cont_porcentagem
        text       = vl_texto.

    ADD 1 TO cont.

    WAIT UP TO 1 SECONDS.

  ENDDO.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_TOKEN  text
*----------------------------------------------------------------------*
FORM fm_get_token USING p_token.

  TYPES: BEGIN OF ty_json_retorno,
           access_token  TYPE string,
           token_type    TYPE string,
           expires_in    TYPE string,
           refresh_token TYPE string,
         END OF ty_json_retorno.

  DATA: lc_retorno     TYPE ty_json_retorno,
        lo_http_client TYPE REF TO if_http_client,
        lo_rest_client TYPE REF TO cl_rest_http_client,
        wa_token       TYPE ztoken,
        lv_resdat      TYPE d,
        lv_restime     TYPE t,
        t_resultado    TYPE  string.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = 'https://hml-autenticacao.tgg.com.br/identity/connect/token'
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  CREATE OBJECT lo_rest_client
    EXPORTING
      io_http_client = lo_http_client.

  lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = if_rest_message=>gc_method_post.

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = 'application/x-www-form-urlencoded'.

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Authorization'
      value = 'Basic aVBvcnRTb2x1dGlvbnNfQXBpX1BhZHJhbzpzZWNyZXQ='.

  lo_http_client->request->set_form_field( EXPORTING name = 'grant_type' value = 'password' ).
  lo_http_client->request->set_form_field( EXPORTING name = 'username' value = 'ffc5339b2ae498c20d70c525fddf1ebe' ).
  lo_http_client->request->set_form_field( EXPORTING name = 'password' value = '6a3ab1f08a779355504a3e54033fc28c' ).
  lo_http_client->request->set_form_field( EXPORTING name = 'scope' value = 'offline_access' ).

  CALL METHOD lo_http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4.

  CALL METHOD lo_http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.

  lo_http_client->response->get_status( IMPORTING code = DATA(return_codet) ).
  t_resultado = lo_http_client->response->get_cdata( ).

  IF return_codet EQ 200.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json        = t_resultado
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = lc_retorno.

*   "// Recupera o TimesTamp do sistema
    GET TIME STAMP FIELD datastamp.

*   "// Adiciona os segundos na data/hora com redução de 5 minutos
    TRY.
        CALL METHOD cl_abap_tstmp=>td_add
          EXPORTING
            date     = sy-datum
            time     = sy-uzeit
            secs     = CONV #( lc_retorno-expires_in - 300 )
          IMPORTING
            res_date = lv_resdat
            res_time = lv_restime.

      CATCH cx_parameter_invalid_type .
      CATCH cx_parameter_invalid_range .
    ENDTRY.

*   "// Fixa o Vencimento do Token
    wa_token =
      VALUE #(
                modulo = 'ACESSO_TGG'
                token = lc_retorno-access_token
                timestamp = datastamp
                data = lv_resdat
                hora = lv_restime
              ).

    p_token = |Bearer { lc_retorno-access_token }|.

    MODIFY ztoken FROM wa_token.
    COMMIT WORK.


  ENDIF.

ENDFORM.

**&---------------------------------------------------------------------*
**&      Form  FM_GET_ACESSO_API
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_E_TOKEN  text
***----------------------------------------------------------------------*
*FORM fm_get_acesso_api USING p_token.
*
*
*  DATA: lo_http_client TYPE REF TO if_http_client,
*        lo_rest_client TYPE REF TO cl_rest_http_client,
*        vl_url         TYPE string,
*        dataminima     TYPE c LENGTH 10,
*        datamaxima     TYPE c LENGTH 10,
*        cpf14          TYPE char14,
*        cpf11          TYPE char11,
*        t_saida        TYPE string,
*        t_resultado    TYPE string,
*        seq            TYPE numc15,
*        wa_0025        TYPE zhcmt_py_0025,
*        it_0025        TYPE TABLE OF zhcmt_py_0025,
*        lc_retorno     TYPE zhcme_py_0001.
*
*  dataminima = |{ o_dai-low(4) }/{ o_dai-low+4(2) }/{ o_dai-low+6(2) }|.
*  datamaxima = |{ o_daf-low(4) }/{ o_daf-low+4(2) }/{ o_daf-low+6(2) }|.
*
*  vl_url = 'https://hml-api.tgg.com.br/InterfaceRegistroAcesso?localEspecifico=1'.
*
*  IF dataminima IS NOT INITIAL.
*    vl_url = |{ vl_url }&dataMinima={ dataminima }|.
*  ENDIF.
*
*  IF datamaxima IS NOT INITIAL.
*    vl_url = |{ vl_url }&dataMaxima={ datamaxima }|.
*  ENDIF.
*
*  IF o_cpf-low IS NOT INITIAL.
*
*    cpf14 = o_cpf-low.
*    REPLACE ALL OCCURRENCES OF '.' IN cpf14 WITH ''.
*    REPLACE ALL OCCURRENCES OF '-' IN cpf14 WITH ''.
*    cpf11 = cpf14.
*
*    vl_url = |{ vl_url }&documento={ cpf11 }|.
*
*  ENDIF.
*
*  CASE abap_true.
*    WHEN p_fun.
*      vl_url = |{ vl_url }&apenasFuncionarios=1|.
*    WHEN p_tod.
*      vl_url = |{ vl_url }&apenasFuncionarios=2|.
*  ENDCASE.
*
*  CALL METHOD cl_http_client=>create_by_url
*    EXPORTING
*      url                = vl_url
*    IMPORTING
*      client             = lo_http_client
*    EXCEPTIONS
*      argument_not_found = 1
*      plugin_not_active  = 2
*      internal_error     = 3
*      OTHERS             = 4.
*
*  CREATE OBJECT lo_rest_client
*    EXPORTING
*      io_http_client = lo_http_client.
*
*  CALL METHOD lo_http_client->request->set_version
*    EXPORTING
*      version = if_http_request=>co_protocol_version_1_0.
*
*  CALL METHOD lo_http_client->request->set_header_field
*    EXPORTING
*      name  = '~request_method'
*      value = if_rest_message=>gc_method_get.
*
*  CALL METHOD lo_http_client->request->set_header_field
*    EXPORTING
*      name  = 'Content-Type'
*      value = 'application/x-www-form-urlencoded'.
*
*  CALL METHOD lo_http_client->request->set_header_field
*    EXPORTING
*      name  = 'Authorization'
*      value = p_token.
*
*  CALL METHOD lo_http_client->send
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      http_invalid_timeout       = 4
*      OTHERS                     = 5.
*
*  IF sy-subrc IS NOT INITIAL.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  CALL METHOD lo_http_client->receive
*    EXCEPTIONS
*      http_communication_failure = 1
*      http_invalid_state         = 2
*      http_processing_failed     = 3
*      OTHERS                     = 4.
*
*  IF sy-subrc IS NOT INITIAL.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  CALL METHOD lo_http_client->response->get_status
*    IMPORTING
*      code = DATA(return_codet).
*
*  t_resultado = lo_http_client->response->get_cdata( ).
*
*  IF return_codet EQ 200.
*
*    CALL METHOD /ui2/cl_json=>deserialize
*      EXPORTING
*        json        = t_resultado
*        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*      CHANGING
*        data        = lc_retorno.
*
*    LOOP AT lc_retorno-value INTO DATA(w_value).
*
*      IF w_value-documento IS NOT INITIAL.
*        CLEAR cpf14.
*
*        cpf11 = w_value-documento.
*
*        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
*          EXPORTING
*            input  = cpf11   " CPF in internal format (NUMC 11)
*          IMPORTING
*            output = cpf14.  " CPF in screen format (999.999.999-99)
*
*      ENDIF.
*
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          nr_range_nr             = '01'
*          object                  = 'ZSEQ_TGG'
*        IMPORTING
*          number                  = seq
*        EXCEPTIONS
*          interval_not_found      = 1
*          number_range_not_intern = 2
*          object_not_found        = 3
*          quantity_is_0           = 4
*          quantity_is_not_1       = 5
*          interval_overflow       = 6
*          buffer_overflow         = 7
*          OTHERS                  = 8.
*
*      SELECT SINGLE pernr
*        FROM pa0465
*        INTO @DATA(vl_pernr)
*        WHERE cpf_nr EQ @cpf14.
*
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE bukrs, werks
*          FROM pa0001
*          INTO @DATA(wa_pa0001)
*          WHERE pernr EQ @vl_pernr.
*      ENDIF.
*
*      APPEND
*      VALUE #(
*                seq          = seq
*                data_inicio  = o_dai-low
*                data_fim     = o_daf-low
*                cod_retorno  = w_value-codigoretorno
*                msg_retorno  = w_value-mensagemretorno
*                bukrs        = wa_pa0001-bukrs
*                werks        = wa_pa0001-werks
*                pernr        = vl_pernr
*                cname        = |{ w_value-nome CASE = UPPER }|
*                nr_cpf       = cpf14
*                perfil       = w_value-perfilacesso
*                qtde_acessos = w_value-qtdeacesso
*                user_name    = sy-uname
*                data         = sy-datum
*                hora         = sy-uzeit
*             ) TO it_0025.
*
*      CLEAR: vl_pernr, wa_pa0001, cpf11.
*
*    ENDLOOP.
*
*    MODIFY zhcmt_py_0025 FROM TABLE it_0025.
*    COMMIT WORK.
*
*  ENDIF.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_EXECUTAR_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_executar_api USING p_direcao.

*  PERFORM fm_indicador USING 'Conexão com API Acesso'.
*  Cadastro da API ZLES0096

  DATA: wa_token TYPE ztoken,
        e_token  TYPE string.

  DATA: go_int           TYPE REF TO zcl_integracao_iportsolution,
        wa_zhcme_py_0004 TYPE  zhcme_py_0004.

  CASE p_direcao.
    WHEN 1.

      SELECT SINGLE *
        FROM zhcmt0004
        INTO @DATA(wa_zhcmt0004)
        WHERE anopr EQ @o_ano-low
          AND mespr EQ @o_mes-low.

      wa_zhcme_py_0004 =
      VALUE #(
                dataminima        = wa_zhcmt0004-initm
                datamaxima        = wa_zhcmt0004-fimtm
                documento         = o_cpf-low
                apenasfuncionario = p_fun
                localespecifico   = 1
             ).

      CREATE OBJECT go_int
        EXPORTING
          i_servico    = 'TGG_REFEITORIO_ACESSO'
          i_parametros = wa_zhcme_py_0004.

      go_int->zif_integracao_iportsolution~get_acesso( ).

    WHEN 2.

      CREATE OBJECT go_int
        EXPORTING
          i_servico = 'TGG_REFEITORIO_REQUISITO'.

      go_int->zif_integracao_iportsolution~set_ausencia( ).

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ENVIAR_AUSENCIA_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_0003  text
*----------------------------------------------------------------------*
FORM fm_enviar_ausencia_api USING p_token.


  DATA: lo_http_client TYPE REF TO if_http_client,
        lo_rest_client TYPE REF TO cl_rest_http_client,
        vl_url         TYPE string,
        json           TYPE string,
        tam_json_s     TYPE string, "Tamanho do Arquivo JSON
        tam_json_i     TYPE i,
        t_resultado    TYPE string.

  vl_url = 'https://hml-api.tgg.com.br/InterfacePessoaFisicaRestricaoRequisito'.

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = vl_url
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  CREATE OBJECT lo_rest_client
    EXPORTING
      io_http_client = lo_http_client.

  CALL METHOD lo_http_client->request->set_version
    EXPORTING
      version = if_http_request=>co_protocol_version_1_0.

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = if_rest_message=>gc_method_get.

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = 'application/json'.

  CALL METHOD lo_http_client->request->set_header_field
    EXPORTING
      name  = 'Authorization'
      value = p_token.

  LOOP AT it_0003 INTO wa_0003.

    json = '{' &&
              '"Documento": "'          && wa_0003-_documento          && '",' &&
              '"RestricaoRequisito": "' && wa_0003-_restricao_requisito && '",' &&
              '"DataInicio": "'         && wa_0003-_data_inicio         && '",' &&
              '"DataTermino": "'        && wa_0003-_data_termino        && '"'  &&
            '}'.

    tam_json_i = strlen( json ).
    tam_json_s = tam_json_i.

    CALL METHOD lo_http_client->request->set_header_field
      EXPORTING
        name  = 'Content-Length'
        value = tam_json_s.

    CALL METHOD lo_http_client->request->set_cdata
      EXPORTING
        data   = json
        offset = 0
        length = tam_json_i.

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD lo_http_client->response->get_status
      IMPORTING
        code = DATA(return_codet).

    t_resultado = lo_http_client->response->get_cdata( ).

*  IF return_codet EQ 200.
*
*    CALL METHOD /ui2/cl_json=>deserialize
*      EXPORTING
*        json        = t_resultado
*        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*      CHANGING
*        data        = lc_retorno.
*  ENDIF.


  ENDLOOP.


ENDFORM.
