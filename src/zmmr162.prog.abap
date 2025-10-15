*&---------------------------------------------------------------------*
*& Report  ZMMR162
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr162.

TABLES zmmt0008.


TYPES: BEGIN OF ty_dados_api_c,
         placa              TYPE string,
         instrucao          TYPE string,
         local_carregamento TYPE string,
         quantidade         TYPE kwmeng,  " ZMMT0008-MENGE,
         notas              TYPE string,
         cancelado          TYPE string,
       END OF  ty_dados_api_c.

TYPES: BEGIN OF ty_dados_api,
         placa              TYPE string,
         instrucao          TYPE string,
         local_carregamento TYPE string,
         quantidade         TYPE kwmeng,
         notas              TYPE string,
       END OF  ty_dados_api.

TYPES: BEGIN OF ty_zmmt0008,
         vbeln              TYPE zmmt0008-vbeln,
         vbeln_vf           TYPE zmmt0008-vbeln_vf,
         placa_cav          TYPE zmmt0008-placa_cav,
         nfnum              TYPE zmmt0008-nfnum,
         lgort              TYPE zsdt0166-lote,
         werks              TYPE zmmt0008-werks,
         menge              TYPE zmmt0008-menge,
         integ_rondonline   TYPE zmmt0008-integ_rondonline,
         instrucao          TYPE string,
         local_carregamento TYPE string,
         refkey             TYPE j_1bnflin-refkey,
         status             TYPE zmmt0008-status,
       END OF ty_zmmt0008.


TYPES: BEGIN OF ty_zmmt0008_aux.
         INCLUDE STRUCTURE zmmt0008.
TYPES: END OF ty_zmmt0008_aux.

*-BUG 64769 - 23.08.2021 - JT - inicio
TYPES: BEGIN OF ty_jlin,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         reftyp TYPE j_1bnflin-reftyp,
         refkey TYPE j_1bnflin-refkey,
         nfenum TYPE j_1bnfdoc-nfenum.
TYPES: END   OF ty_jlin.
*-BUG 64769 - 23.08.2021 - JT - fim


DATA: git_dados_api_c  TYPE TABLE OF ty_dados_api_c,
      gwa_dados_api_c  TYPE ty_dados_api_c,

      git_dados_api    TYPE TABLE OF ty_dados_api,
      gwa_dados_api    TYPE ty_dados_api,

      git_zmmt0008     TYPE TABLE OF ty_zmmt0008,
      gwa_zmmt0008     TYPE  ty_zmmt0008,
      git_zsdt0066     TYPE TABLE OF zsdt0066,
      gwa_zsdt0066     TYPE zsdt0066,
      git_zmmt0008_aux TYPE TABLE OF zmmt0008,
      gwa_zmmt0008_aux TYPE zmmt0008,
      git_i_zmmt0008   TYPE TABLE OF zmmt0008, "pbi - 64946 - cbrand
      git_zmmt8        TYPE TABLE OF zmmt0008,
      gwa_zmmt8        TYPE zmmt0008,
      git_zsdt0166     TYPE TABLE OF zsdt0166,
      gwa_zsdt0166     TYPE zsdt0166,
      git_jlin         TYPE TABLE OF ty_jlin,
      gwa_jlin         TYPE ty_jlin.

DATA: ob_web_service TYPE REF TO zcl_webservice_trace.

DATA: lv_json       TYPE string,
      gva_json      TYPE string,
      return_code   TYPE i,
      id_referencia TYPE char11,
      v_token       TYPE string,
      v_url         TYPE string.

DATA: l_id_integracao  TYPE zintegracao-id_integracao,
      l_url            TYPE string,
      w_integracao     TYPE zintegracao,
      w_integracao_log TYPE zintegracao_log.

DATA: git_canc_qnt   TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      gva_por_qnt(1) TYPE c,
      l_erro         TYPE char1.

*---------------------------------------------------------------------*
* TAG                                                                 *
*---------------------------------------------------------------------*
DEFINE add_tag.
  CONCATENATE gva_json '' &1 ':' &2 '' &3  INTO gva_json.
END-OF-DEFINITION.

DEFINE add_tag_string.
  CONCATENATE gva_json '' &1 ':'  '"' &2 '"' '' &3 INTO gva_json.
END-OF-DEFINITION.

CREATE OBJECT ob_web_service.

INITIALIZATION.


  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: s_werks  FOR zmmt0008-werks,
                    s_lgort  FOR zmmt0008-lgort,
                    s_vbn_vf FOR zmmt0008-vbeln_vf,
                    s_plcav  FOR zmmt0008-placa_cav.
  SELECTION-SCREEN END OF BLOCK b1.

  PARAMETERS: s_acao(1) TYPE c NO-DISPLAY .

START-OF-SELECTION.

  PERFORM z_seleciona_dados.
  PERFORM z_trata_dados.
  PERFORM z_api.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_dados .

  FREE: git_jlin.

*** SET - PBI - 64946 - Inicio - CSB
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZMMT0008_CANCELADO'
    TABLES
      set_values    = git_canc_qnt
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  READ TABLE git_canc_qnt INTO DATA(gwa_canc_qnt).
  IF sy-subrc  EQ 0.
    gva_por_qnt = gwa_canc_qnt-from.
  ENDIF.
*** SET - PBI - 64946 - Fim  - CSB

  IF sy-batch = 'X'.

    SELECT *
    FROM zmmt0008 INTO TABLE git_zmmt0008_aux
    WHERE status IN ('1','3')
      AND vbeln_vf NE ''.
*** PBI - 64946 - CSB
*      SELECT *
*      FROM zmmt0008 INTO TABLE git_zmmt0008_aux
*      WHERE integ_rondonline EQ ' '
*        AND vbeln_vf NE ''.

*** Bug - 53371 - Inicio
  ELSE.
    IF s_acao = 'I'. "Integrar
      SELECT *
      FROM zmmt0008 INTO TABLE git_zmmt0008_aux
      WHERE status    EQ '1'
        AND werks     IN s_werks
        AND lgort     IN s_lgort
        AND vbeln_vf  IN s_vbn_vf
        AND placa_cav IN s_plcav.
    ELSE.
      IF s_acao = 'E'.
        SELECT *
          FROM zmmt0008 INTO TABLE git_zmmt0008_aux
          WHERE status EQ '3'
           AND werks     IN s_werks
           AND lgort     IN s_lgort
           AND vbeln_vf  IN s_vbn_vf
           AND placa_cav IN s_plcav.
      ENDIF.
    ENDIF.
  ENDIF.
*** Bug - 53371 - Fim

  SORT git_zmmt0008_aux BY vbeln vbeln_vf.
  DATA: lva_date TYPE sy-datum.

  LOOP AT git_zmmt0008_aux INTO gwa_zmmt0008_aux.
* PBI - 64946 - Inicio - CSB
    lva_date = gwa_zmmt0008_aux-dt_inicial_integ.
    lva_date = lva_date + 5.
    IF lva_date < sy-datum.
      gwa_zmmt0008_aux-status = '5' .
      gwa_zmmt0008_aux-integ_rondonline = '9999999999' .
      MODIFY zmmt0008  FROM gwa_zmmt0008_aux.
* PBI - 64946 - Fim - CSB
    ELSE.
      gwa_zmmt0008-vbeln     = gwa_zmmt0008_aux-vbeln.
      gwa_zmmt0008-vbeln_vf  = gwa_zmmt0008_aux-vbeln_vf.
      gwa_zmmt0008-placa_cav = gwa_zmmt0008_aux-placa_cav.
      gwa_zmmt0008-nfnum     = gwa_zmmt0008_aux-nfnum.
      gwa_zmmt0008-werks     = gwa_zmmt0008_aux-werks.
      gwa_zmmt0008-lgort     = gwa_zmmt0008_aux-lgort.
      gwa_zmmt0008-menge     = gwa_zmmt0008_aux-menge.
*-BUG 64769 - 23.08.2021 - JT - inicio
      gwa_zmmt0008-refkey    = gwa_zmmt0008-vbeln_vf.
*-BUG 64769 - 23.08.2021 - JT - fim
      gwa_zmmt0008-status    = gwa_zmmt0008_aux-status. "PBI 64946 - CSB
      COLLECT  gwa_zmmt0008 INTO git_zmmt0008.

      CLEAR: gwa_zmmt0008, gwa_zmmt0008_aux.
    ENDIF.
    CLEAR: lva_date.
  ENDLOOP.

  IF git_zmmt0008[] IS NOT INITIAL.

*-BUG 64769 - 23.08.2021 - JT - inicio
    SELECT j_1bnflin~docnum j_1bnflin~itmnum
           j_1bnflin~reftyp j_1bnflin~refkey j_1bnfdoc~nfenum
      FROM j_1bnflin
     INNER JOIN j_1bnfdoc ON j_1bnfdoc~docnum = j_1bnflin~docnum
      INTO TABLE git_jlin
       FOR ALL ENTRIES IN git_zmmt0008
     WHERE j_1bnflin~refkey = git_zmmt0008-refkey.
*-BUG 64769 - 23.08.2021 - JT - fim

    SELECT * FROM zsdt0066 INTO TABLE git_zsdt0066
      FOR ALL ENTRIES IN git_zmmt0008
     WHERE vbeln EQ git_zmmt0008-vbeln.

    SELECT * FROM zsdt0166 INTO TABLE git_zsdt0166
      FOR ALL ENTRIES IN git_zmmt0008
     WHERE lote   EQ git_zmmt0008-lgort
      AND  werks  EQ git_zmmt0008-werks
      AND  status EQ 'A'.

  ENDIF.

*** Bug - 53371 - Inicio
  "SORT git_zsdt0166 BY data DESCENDING.
  SORT git_zsdt0166 BY  id DESCENDING.
*** Bug - 53371 - Fim

*-BUG 64769 - 23.08.2021 - JT - inicio
  SORT git_jlin BY refkey.
  DELETE ADJACENT DUPLICATES FROM git_jlin
                        COMPARING refkey.
*-BUG 64769 - 23.08.2021 - JT - fim

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_trata_dados .

  TYPES: tt_zmmt0008 TYPE TABLE OF ty_zmmt0008_aux WITH EMPTY KEY.

  LOOP AT git_zmmt0008 INTO gwa_zmmt0008.

    CLEAR: gwa_dados_api_c, gwa_zsdt0066, gwa_zsdt0166.

    READ TABLE git_zsdt0066 INTO gwa_zsdt0066 WITH KEY vbeln = gwa_zmmt0008-vbeln.
    IF sy-subrc EQ 0.
      gwa_dados_api_c-instrucao = gwa_zsdt0066-instrucao.
    ENDIF.

*** Bug - 53371 - Inicio
    READ TABLE git_zsdt0166 INTO gwa_zsdt0166 WITH KEY werks  = gwa_zmmt0008-werks
                                                       lote   = gwa_zmmt0008-lgort
                                                       status = 'A'.
    IF sy-subrc EQ 0.
      gwa_dados_api_c-local_carregamento = gwa_zsdt0166-algodoeira.
    ENDIF.

*-BUG 64769 - 23.08.2021 - JT - inicio
    CLEAR gwa_jlin.
    READ TABLE git_jlin INTO gwa_jlin WITH KEY refkey = gwa_zmmt0008-refkey
                                      BINARY SEARCH.
*-BUG 64769 - 23.08.2021 - JT - fim

*    READ TABLE git_zsdt0166 INTO gwa_zsdt0166 INDEX 1.
*    IF sy-subrc EQ 0.
*      gwa_dados_api_c-local_carregamento = gwa_zsdt0166-algodoeira.
*    ENDIF.
*** Bug - 53371 - Fim

    gwa_dados_api_c-placa       =  gwa_zmmt0008-placa_cav.
*-BUG 64769 - 23.08.2021 - JT - inicio
    gwa_dados_api_c-notas       =  gwa_jlin-nfenum.  "gwa_zmmt0008-nfnum.
*-BUG 64769 - 23.08.2021 - JT - fim

*** Bug - 53371 - Inicio
    DATA(lva_count) = lines( VALUE tt_zmmt0008( FOR line IN git_zmmt0008_aux WHERE ( werks    EQ gwa_zmmt0008-werks
                                                                                AND  lgort    EQ gwa_zmmt0008-lgort
                                                                                AND  vbeln_vf EQ gwa_zmmt0008-vbeln_vf  ) ( line ) ) ).
* PBI - 64946 - Inicio - CSB
    IF gwa_zmmt0008-status = '3'.
      IF gva_por_qnt = 'X'.
        gwa_dados_api_c-quantidade  = lva_count * -1.
      ELSE.
        gwa_dados_api_c-quantidade  = 0.
        gwa_dados_api_c-cancelado  = 'X'.
      ENDIF.
    ELSE.
      gwa_dados_api_c-quantidade  = lva_count.
    ENDIF.
*    gwa_dados_api-quantidade  =  gwa_zmmt0008-menge.
*** Bug - 53371 - fim

    APPEND gwa_dados_api_c TO git_dados_api_c.

    MOVE-CORRESPONDING gwa_zmmt0008 TO gwa_zmmt8.
    APPEND gwa_zmmt8 TO git_zmmt8.

    CLEAR: gwa_zmmt0008, gwa_zmmt8.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_API
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_api .

  DATA: lva_block_werks TYPE zmmt0008-werks,
        lva_block_lgort TYPE zmmt0008-lgort,
        lva_block_charg TYPE zmmt0008-charg,
        lva_qnt         TYPE string.

  LOOP AT git_dados_api_c INTO gwa_dados_api_c.

    CLEAR: gwa_zmmt8, lva_qnt, git_i_zmmt0008.

    LOOP AT git_zmmt0008_aux INTO gwa_zmmt0008_aux WHERE placa_cav    =  gwa_dados_api_c-placa
                                                     AND nfnum        =  gwa_dados_api_c-notas.

      lva_block_werks = gwa_zmmt0008_aux-werks.
      lva_block_lgort = gwa_zmmt0008_aux-lgort.
      lva_block_charg = gwa_zmmt0008_aux-charg.

      CALL FUNCTION 'ENQUEUE_EZMMT0008'
        EXPORTING
          mandt          = sy-mandt
          werks          = lva_block_werks
          lgort          = lva_block_lgort
          charg          = lva_block_charg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      APPEND gwa_zmmt0008_aux TO git_i_zmmt0008.
      CLEAR: gwa_zmmt0008_aux.

    ENDLOOP.
    "{"PLACA":"NCE7535","INSTRUCAO":"AGR125-18.02","LOCAL_CARREGAMENTO":"TUCUNARÉ","QUANTIDADE":3.000,"NOTAS":"000116919","CANCELADO":""}

    CLEAR: gva_json.

    IF gva_por_qnt = 'X'.
      MOVE-CORRESPONDING gwa_dados_api_c TO gwa_dados_api.

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data   = gwa_dados_api
        RECEIVING
          r_json = gva_json.

    ELSE.
      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data   = gwa_dados_api_c
        RECEIVING
          r_json = gva_json.
    ENDIF.

    l_erro = abap_false.

    TRY .
        zcl_integracao_rond_estoque=>zif_integracao_rond_estoque~get_instance(
          )->set_int_estoque( i_json = gva_json i_zmmt0008_t = git_i_zmmt0008 ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        l_erro = abap_true.
        ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_error).    "  "
        l_erro = abap_true.
        ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

*-CS2023000189-18.05.2023-#108750-JT-inicio
    PERFORM f_gera_log.
*-CS2023000189-18.05.2023-#108750-JT-fim

    LOOP AT git_zmmt0008_aux INTO gwa_zmmt0008_aux WHERE placa_cav    =  gwa_dados_api-placa
                                                     AND nfnum        =  gwa_dados_api-notas.

      lva_block_werks = gwa_zmmt0008_aux-werks.
      lva_block_lgort = gwa_zmmt0008_aux-lgort.
      lva_block_charg = gwa_zmmt0008_aux-charg.

      CALL FUNCTION 'DEQUEUE_EZMMT0008'
        EXPORTING
          mandt          = sy-mandt
          werks          = lva_block_werks
          lgort          = lva_block_lgort
          charg          = lva_block_charg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    ENDLOOP.

    CLEAR gwa_dados_api.
  ENDLOOP.

****************************** Código Antigo Abaixo
*** PBI - 64946.
*  SELECT SINGLE * FROM zauth_webservice INTO @DATA(ws_service)
*             WHERE service = 'RONDONLINE'.
*
*  IF  sy-subrc EQ 0.
*    v_url   =  ws_service-url.
*  ENDIF.
*
*  CALL METHOD cl_http_client=>create_by_url
*    EXPORTING
*      url                = v_url
*      ssl_id             = 'DFAULT' "ME->AT_PAR_AUTENTICACAO-SSL_ID
*    IMPORTING
*      client             = DATA(http_client)
*    EXCEPTIONS
*      argument_not_found = 1
*      plugin_not_active  = 2
*      internal_error     = 3
*      OTHERS             = 4.
*
*  l_url = v_url.
*
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = '~request_method'
*      value = 'POST'.
*
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = '~server_protocol'
*      value = 'HTTP/1.1'.
*
*  v_token   =  |{ 'Bearer' } { ws_service-add01 }|.
*
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = 'Authorization'
*      value = v_token.
*
*  CALL METHOD http_client->request->set_header_field
*    EXPORTING
*      name  = 'Content-Type'
*      value = 'application/json'.
*
*  CLEAR gwa_dados_api.
*
*  LOOP AT git_dados_api INTO gwa_dados_api.
*
*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data   = gwa_dados_api
*      RECEIVING
*        r_json = lv_json.
*
*    CALL METHOD http_client->request->set_cdata
*      EXPORTING
*        data = lv_json.
*
*    http_client->send( ).
*
*    CALL METHOD http_client->receive
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        OTHERS                     = 4.
*
*    http_client->response->get_status( IMPORTING code = return_code ).
*
*    CASE return_code.
*      WHEN 201 OR 202.
*
*        CLEAR: gwa_zmmt8.
*        READ TABLE git_zmmt8 INTO gwa_zmmt8 WITH KEY   placa_cav    =  gwa_dados_api-placa
*                                                       nfnum        =  gwa_dados_api-notas.
*        "menge        =  gwa_dados_api-quantidade
*
*        IF sy-subrc EQ 0.
*
*          PERFORM f_inclui_integracao CHANGING l_id_integracao.
*
*          IF s_acao = 'I'.
*
*            UPDATE zmmt0008
*            SET status = '2' "(Integrado)
*            WHERE werks     = gwa_zmmt8-werks
*            AND   lgort     = gwa_zmmt8-lgort
*            AND   vbeln_vf  = gwa_zmmt8-vbeln_vf.
*
*          ELSE.
*            IF s_acao = 'E'.
*              UPDATE zmmt0008
*              SET integ_rondonline = l_id_integracao
*              WHERE werks     = gwa_zmmt8-werks
*              AND   lgort     = gwa_zmmt8-lgort
*              AND   vbeln_vf  = gwa_zmmt8-vbeln_vf.
*            ENDIF.
*          ENDIF.
*          COMMIT WORK.
*        ENDIF.
*
*      WHEN OTHERS.
*
*        id_referencia = 'ZRON9999999'.
*
*        CALL METHOD ob_web_service->envia_integracao
*          EXPORTING
*            e_client      = CAST #( http_client )
*            id_referencia = id_referencia
*            id_interface  = '035'.
*
*    ENDCASE.
*
*    CLEAR gwa_dados_api.
*
*  ENDLOOP.


ENDFORM.

*-CS2023000189-18.05.2023-#108750-JT-inicio
********************************************************
* gerar log
********************************************************
FORM f_gera_log.

  DATA: l_mensagem  TYPE string.

*-----------------------------------
*-- gerar log envio
*-----------------------------------
  LOOP AT git_i_zmmt0008 INTO gwa_zmmt0008_aux.
    SELECT SINGLE ch_referencia
      INTO @DATA(l_ch_ref)
      FROM zsdt0001
     WHERE tp_movimento = 'S'
       AND nr_romaneio  = @gwa_zmmt0008_aux-nr_romaneio
       AND branch       = @gwa_zmmt0008_aux-werks
       AND vbeln        = @gwa_zmmt0008_aux-vbeln.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM zsdt0330
      INTO @DATA(w_zsdt0330)
     WHERE ch_referencia  = @l_ch_ref
       AND werks          = @gwa_zmmt0008_aux-werks
       AND lgort          = @gwa_zmmt0008_aux-lgort
       AND acharg         = @gwa_zmmt0008_aux-charg
       AND cancelado      = @abap_false.

    CHECK sy-subrc = 0.

    l_mensagem = COND #( WHEN l_erro = abap_false THEN 'Integrado com Sucesso ao Rondoline'
                                                  ELSE 'Não foi possivel integrar ao Rondoline' ).

    zcl_trace_cotton=>zif_trace_cotton~get_instance(
       )->set_gravar_log( EXPORTING i_id_carga     = w_zsdt0330-id_carga
                                    i_matnr        = w_zsdt0330-matnr
                                    i_werks        = w_zsdt0330-werks
                                    i_lgort        = w_zsdt0330-lgort
                                    i_acharg       = w_zsdt0330-acharg
                                    i_safra        = w_zsdt0330-safra
                                    i_tipo_integra = 'GL'
                                    i_tipo_msg     = 'S'
                                    i_mensagem     = l_mensagem ).

    DATA(l_status_api) = COND #( WHEN l_erro = abap_true THEN abap_off
                                                         ELSE abap_on ).

    UPDATE zsdt0330 SET status_api_gl    = l_status_api
                  WHERE id_carga         = w_zsdt0330-id_carga
                    AND matnr            = w_zsdt0330-matnr
                    AND werks            = w_zsdt0330-werks
                    AND lgort            = w_zsdt0330-lgort
                    AND acharg           = w_zsdt0330-acharg
                    AND safra            = w_zsdt0330-safra
                    AND cancelado        = abap_off.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.
*-CS2023000189-18.05.2023-#108750-JT-fim

*&---------------------------------------------------------------------*
*&      Form  F_INCLUI_INTEGRACAO
*&---------------------------------------------------------------------*
FORM f_inclui_integracao  CHANGING p_id_integracao.

  DATA: l_id     TYPE zde_id_integracao.

  CLEAR: w_integracao,
         w_integracao_log,
         p_id_integracao.


*--------------------------------------
* sequencia id integracao
*--------------------------------------
  zcl_integracao=>zif_integracao~get_instance( )->get_new_id_integracao(
      IMPORTING e_id_integracao = l_id ).

  p_id_integracao = l_id.

*--------------------------------------
* cria header
*--------------------------------------
  CLEAR w_integracao.

  w_integracao-mandt                = sy-mandt.
  w_integracao-id_integracao        = l_id.
  w_integracao-id_interface         = '035'.
  w_integracao-dt_registro          = sy-datum.
  w_integracao-hr_registro          = sy-uzeit.
  w_integracao-us_registro          = sy-uname.
  w_integracao-id_interface         = '035'.
  w_integracao-ck_integrado         = abap_true.
  w_integracao-dt_integrado         = sy-datum.
  w_integracao-hr_integrado         = sy-uzeit.
  w_integracao-us_integrado         = sy-uname.
  w_integracao-ds_url               = l_url.
  w_integracao-ds_body              = lv_json.
  w_integracao-ds_metodo            = 'POST'.
  w_integracao-ds_content_type      = 'application/json; charset=UTF-8'.
  w_integracao-ds_formato           = 'JSON'.
  w_integracao-ds_funcao_processa   = '/login'.

  CONCATENATE  '[#{#"name":"~request_uri",#"value":"/login"#}#,#{#"name":"~request_method",#"value":"POST"#}#,#'
               '{#"name":"~server_protocol",#"value":"HTTP/1.1"#}#,#{#"name":"content-type",#"value":"application/json; charset=UTF-8"#}#,'
               '#"name":"content-length",#"value":"67 "#}##]#'
         INTO w_integracao-ds_header.
  w_integracao-nm_code              = return_code.
  w_integracao-ds_data_retorno      = lv_json.
  w_integracao-tp_integracao        = '0'.
  w_integracao-tp_sincronia         = '1'.
  w_integracao-ck_retornou          = abap_true.
  w_integracao-ck_processado        = abap_true.
  w_integracao-dt_processado        = sy-datum.
  w_integracao-hr_processado        = sy-uzeit.
  w_integracao-us_processado        = sy-uname.
  MODIFY zintegracao             FROM w_integracao.

*--------------------------------------
* cria header LOG
*--------------------------------------
  CLEAR w_integracao_log.

  w_integracao_log-mandt            = sy-mandt.
  w_integracao_log-id_integracao    = l_id.
  w_integracao_log-dt_registro      = sy-datum.
  w_integracao_log-hr_registro      = sy-uzeit.
  w_integracao_log-us_registro      = sy-uname.
  w_integracao_log-nm_code          = return_code.
  w_integracao_log-ds_data_retorno  = 'Sucesso'.
  w_integracao_log-dt_resposta      = sy-datum.
  w_integracao_log-hr_resposta      = sy-uzeit.
  w_integracao_log-us_resposta      = sy-uname.
  MODIFY zintegracao_log         FROM w_integracao_log.

ENDFORM.
