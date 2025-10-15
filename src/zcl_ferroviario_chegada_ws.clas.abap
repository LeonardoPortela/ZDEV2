class ZCL_FERROVIARIO_CHEGADA_WS definition
  public
  create public .

public section.

  methods SET_SRV_INTEGRACAO
    importing
      !I_SRV_INTEGRACAO type ZDE_SRV_INTEGRACAO .
  methods SET_DT_CHEGADA
    importing
      !I_DT_CHEGADA type ZDE_DT_CHEGADA .
  methods SET_SIGLA_TERMINAL
    importing
      !I_SIGLA_TERMINAL type ZDE_SIGLA_TERMINAL .
  methods CONSULTAR
    importing
      !I_GRAVAR type CHAR01 optional
    returning
      value(R_OK) type CHAR01
    exceptions
      HTTP_COMMUNICATION_FAILURE
      HTTP_INVALID_STATE
      HTTP_PROCESSING_FAILED
      HTTP_INVALID_TIMEOUT
      ZCX_ENVIO .
  methods GRAVAR_DADOS
    returning
      value(R_GRAVOU) type CHAR01 .
  methods GET_DADOS_CONSULTA
    exporting
      !E_DADOS_DESCARGA type ZLEST0179_TMP_T
      !E_DADOS_NF type ZLEST0180_TMP_T .
  methods PREPARAR_DADOS_GRAVACAO
    returning
      value(R_OK) type CHAR01 .
  methods TRATAR_RETORNO_02
    importing
      !I_CDATA_RETORNO type STRING
    changing
      !C_XML_RETURN type ref to CL_XML_DOCUMENT .
  methods MONTA_XML_02
    returning
      value(R_XML) type STRING .
protected section.
private section.

  data AT_SRV_INTEGRACAO type ZDE_SRV_INTEGRACAO .
  data AT_DT_CHEGADA type ZDE_DT_CHEGADA .
  data AT_SIGLA_TERMINAL type ZDE_SIGLA_TERMINAL .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_DADOS_DESCARGA type ZLEST0179_TMP_T .

  methods MONTA_XML
    returning
      value(R_XML) type STRING .
  methods MONTA_XML_01
    returning
      value(R_XML) type STRING .
  methods SET_URL
    returning
      value(R_URL) type UI_SRC_URL .
  methods TRATAR_XML
    changing
      !C_XML type STRING .
  methods SET_FIELDS
    changing
      !C_IF_HTTP_CLIENT type ref to IF_HTTP_CLIENT .
  methods TRATAR_RETORNO_CONSULTA
    importing
      !I_CDATA_RETORNO type STRING
    changing
      !C_XML_RETURN type ref to CL_XML_DOCUMENT .
  methods TRATAR_RETORNO_01
    importing
      !I_CDATA_RETORNO type STRING
    changing
      !C_XML_RETURN type ref to CL_XML_DOCUMENT .
  methods CONVERSION_CNPJ_INPUT
    changing
      !I_CNPJ type STRING .
ENDCLASS.



CLASS ZCL_FERROVIARIO_CHEGADA_WS IMPLEMENTATION.


  METHOD consultar.

    DATA: http_client     TYPE REF TO if_http_client,
          xml_return      TYPE REF TO cl_xml_document,
          return_code     TYPE i,
          vl_ini_pos      TYPE i,
          lw_ferro_vli    TYPE string,           "*-CS2024000593-22.11.2024-JT-#157860-inicio
          v_cdata_retorno TYPE string,
          v_xml           TYPE string,
          v_url           TYPE ui_src_url,
          v_msg_show      TYPE c LENGTH 200.

    r_ok = abap_false.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = TEXT-001.

    v_url = me->set_url( ).

    CHECK v_url IS NOT INITIAL.

    "Call service
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = CONV #( v_url )
        ssl_id             = 'DFAULT'
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    me->set_fields( CHANGING c_if_http_client = http_client ).

    v_xml = me->monta_xml( ).

    me->tratar_xml( CHANGING c_xml = v_xml ).

    IF v_xml IS NOT INITIAL.
      CALL METHOD http_client->request->set_cdata
        EXPORTING
          data   = v_xml
          offset = 0
          length = strlen( v_xml ).
    ENDIF.

    CALL METHOD http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        MESSAGE s021 WITH | { v_url } (Send) | RAISING http_communication_failure.
        EXIT.
      WHEN 2.
        http_client->close( ).
        MESSAGE s022 WITH | { v_url } (Send) | RAISING http_invalid_state.
        EXIT.
      WHEN 3.
        http_client->close( ).
        MESSAGE s023 WITH | { v_url } (Send) | RAISING http_processing_failed.
        EXIT.
      WHEN 4.
        http_client->close( ).
        MESSAGE s024 WITH | { v_url } (Send) | RAISING http_invalid_timeout.
        EXIT.
    ENDCASE.

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    CASE sy-subrc.
      WHEN 1.
        http_client->close( ).
        MESSAGE s021 WITH | { v_url } (Receive) |  DISPLAY LIKE 'E' RAISING http_communication_failure.
        EXIT.
      WHEN 2.
        http_client->close( ).
        MESSAGE s023 WITH | { v_url } (Receive) |  DISPLAY LIKE 'E' RAISING http_invalid_state.
        EXIT.
      WHEN 3.
        http_client->close( ).
        MESSAGE s024 WITH | { v_url } (Receive) |  DISPLAY LIKE 'E' RAISING http_processing_failed.
        EXIT.
    ENDCASE.

    "//Check return content
    CREATE OBJECT xml_return.

    CALL METHOD xml_return->parse_string
      EXPORTING
        stream = http_client->response->get_cdata( ).

    http_client->response->get_status( IMPORTING code = return_code ).

    v_cdata_retorno = http_client->response->get_cdata( ).

    IF return_code NE '200'.
      CLEAR: vl_ini_pos.

      IF vl_ini_pos IS INITIAL.
        FIND '(400)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '400'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(401)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '401'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(403)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '403'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(404)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '404'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(422)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '422'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(500)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '500'.
        ENDIF.
      ENDIF.

      IF vl_ini_pos IS INITIAL.
        FIND '(503)' IN v_cdata_retorno MATCH OFFSET vl_ini_pos.
        IF vl_ini_pos IS NOT INITIAL.
          return_code = '503'.
        ENDIF.
      ENDIF.

      CASE return_code.
        WHEN 400.
          MESSAGE '(WS)-Requisição mal formatada!' TYPE 'S' RAISING zcx_envio.
        WHEN 401.
          MESSAGE '(WS)-Requisição requer autenticação!' TYPE 'S' RAISING zcx_envio.
        WHEN 403.
          MESSAGE '(WS)-Requisição não autorizada!' TYPE 'S' RAISING zcx_envio.
        WHEN 404.
          MESSAGE '(WS)-Registro não encontrado!' TYPE 'S' RAISING zcx_envio.
        WHEN 422.
          MESSAGE '(WS)-Erro de negócio!' TYPE 'S' RAISING zcx_envio.
        WHEN 500.
          MESSAGE '(WS)-Erro interno do servidor!' TYPE 'S' RAISING zcx_envio.
        WHEN 503.
          MESSAGE '(WS)-Serviço indisponível!' TYPE 'S' RAISING zcx_envio.
        WHEN OTHERS.
          MESSAGE '(WS)-Consulta não realizada!' TYPE 'S' RAISING zcx_envio.
      ENDCASE.

      RETURN.
    ENDIF.

    me->tratar_retorno_consulta(
      EXPORTING
        i_cdata_retorno = v_cdata_retorno
      CHANGING
        c_xml_return    = xml_return
    ).

    r_ok = abap_true.

  ENDMETHOD.


  method CONVERSION_CNPJ_INPUT.

    REPLACE ALL OCCURRENCES OF '.' IN I_CNPJ WITH SPACE.
    REPLACE ALL OCCURRENCES OF '/' IN I_CNPJ WITH SPACE.
    REPLACE ALL OCCURRENCES OF '-' IN I_CNPJ WITH SPACE.
    CONDENSE I_CNPJ NO-GAPS.

  endmethod.


  method GET_DADOS_CONSULTA.

    E_DADOS_DESCARGA = ME->AT_DADOS_DESCARGA.

  endmethod.


  METHOD gravar_dados.

    DATA: wl_zlest0179 TYPE zlest0179,
          it_zlest0179 TYPE TABLE OF zlest0179,
          wl_zlest0180 TYPE zlest0180,
          it_zlest0180 TYPE TABLE OF zlest0180.

    DATA: it_zlest0180_aux TYPE TABLE OF zde_zlest0180_tmp,   "*-CS2024000593-22.11.2024-JT-#157860-inicio
          wa_zlest0180_aux TYPE zde_zlest0180_tmp,            "*-CS2024000593-22.11.2024-JT-#157860-inicio
          it_zlest0179_aux TYPE TABLE OF zlest0179,           "*-CS2024000593-22.11.2024-JT-#157860-inicio
          wa_zlest0179_aux TYPE zlest0179,                    "*-CS2024000593-22.11.2024-JT-#157860-inicio
          vg_input         TYPE char01.                       "*-CS2024000593-22.11.2024-JT-#157860-inicio

    r_gravou = abap_false.

    IF ( me->at_dados_descarga[] IS INITIAL ).
      MESSAGE s026.
      RETURN.
    ENDIF.

    DATA(_ok) = me->preparar_dados_gravacao( ).

    CHECK _ok EQ abap_true.

    CLEAR: it_zlest0179[], it_zlest0180[].

*-CS2024000593-22.11.2024-JT-#157860-inicio
    FREE: it_zlest0180_aux.
    LOOP AT me->at_dados_descarga ASSIGNING FIELD-SYMBOL(<ws_descarga>).
      APPEND LINES OF <ws_descarga>-notas[] TO it_zlest0180_aux[].
    ENDLOOP.

    SELECT *
      FROM zlest0180 AS a
      INTO TABLE @DATA(tg_zlest0180)
       FOR ALL ENTRIES IN @it_zlest0180_aux[]
     WHERE chave_nf EQ @it_zlest0180_aux-chave_nf.

    LOOP AT me->at_dados_descarga INTO DATA(_wl_descarga).
      CLEAR: vg_input.
      LOOP AT _wl_descarga-notas INTO DATA(_wl_nf).
        READ TABLE tg_zlest0180 INTO DATA(ws_zles00180) WITH KEY chave_nf = _wl_nf-chave_nf.
        IF sy-subrc NE 0.
          CLEAR: wl_zlest0180.
          MOVE-CORRESPONDING _wl_nf TO wl_zlest0180.
          APPEND wl_zlest0180 TO it_zlest0180.
          CLEAR: ws_zles00180.
          vg_input = abap_true.
        ENDIF.
      ENDLOOP.

      IF vg_input EQ abap_true.
        CLEAR: wl_zlest0179.
        MOVE-CORRESPONDING _wl_descarga TO wl_zlest0179.
        APPEND wl_zlest0179 TO it_zlest0179.
      ENDIF.
    ENDLOOP.

*    LOOP AT me->at_dados_descarga INTO DATA(_wl_descarga).
*      CLEAR: wl_zlest0179.
*      MOVE-CORRESPONDING _wl_descarga TO wl_zlest0179.
*      APPEND wl_zlest0179 TO it_zlest0179.
*
*      LOOP AT _wl_descarga-notas INTO DATA(_wl_nf).
*        CLEAR: wl_zlest0180.
*        MOVE-CORRESPONDING _wl_nf TO wl_zlest0180.
*        APPEND wl_zlest0180 TO it_zlest0180.
*      ENDLOOP.
*    ENDLOOP.
*-CS2024000593-22.11.2024-JT-#157860-fim

    IF ( it_zlest0179[] IS NOT INITIAL ).
      MODIFY zlest0179 FROM TABLE it_zlest0179.
    ENDIF.

    IF ( it_zlest0180[] IS NOT INITIAL ).
      MODIFY zlest0180 FROM TABLE it_zlest0180.
    ENDIF.

    r_gravou = abap_true.


  ENDMETHOD.


  METHOD MONTA_XML.

    CASE ME->AT_SRV_INTEGRACAO.
      WHEN '01'. "Terminais VLI
        EXIT.
      WHEN '02'. "Terminais BUNGE
        R_XML = ME->MONTA_XML_02( ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF R_XML IS INITIAL.
      MESSAGE S027.
      RETURN.
    ENDIF.

  ENDMETHOD.


  method MONTA_XML_01.

    DATA: XVALOR                  TYPE STRING,
          V_C_AUX                 TYPE STRING.

    CLEAR: R_XML.

    DEFINE CONC_XML.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE R_XML XVALOR INTO R_XML.
    END-OF-DEFINITION.


*    CONC_XML    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">'.
*    CONC_XML       '<SOAP-ENV:Body>'.
*    CONC_XML          '<Consulta xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.
*    CONC_XML             '<Login>'.
*    CONC_XML                ME->AT_AUTH_WS-USERNAME.
*    CONC_XML             '</Login>'.
*    CONC_XML             '<Senha>'.
*    CONC_XML                ME->AT_AUTH_WS-PASSWORD.
*    CONC_XML             '</Senha>'.
*    CONC_XML             '<CodigoTerminal>'.
*    CONC_XML                ME->AT_SIGLA_TERMINAL.
*    CONC_XML             '</CodigoTerminal>'.
*
*    CONC_XML             '<Data>'.
*      V_C_AUX = ME->AT_DT_SAIDA(4) && '-' && ME->AT_DT_SAIDA+4(2) && '-' && ME->AT_DT_SAIDA+6(2).
*      CONC_XML                V_C_AUX.
*    CONC_XML             '</Data>'.
*
*    CONC_XML          '</Consulta>'.
*    CONC_XML       '</SOAP-ENV:Body>'.
*    CONC_XML    '</SOAP-ENV:Envelope>'.

  endmethod.


  METHOD MONTA_XML_02.
    DATA: XVALOR  TYPE STRING,
          V_C_AUX TYPE STRING.

    CLEAR: R_XML.

    DEFINE CONC_XML.
      CLEAR: XVALOR.
      XVALOR = &1.
      CONCATENATE R_XML XVALOR INTO R_XML.
    END-OF-DEFINITION.

    CONC_XML '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">'.
    CONC_XML '<soap:Header>'.
    CONC_XML    '<WsAuthHeader xmlns="http://www.unisolution.com.br/">'.
    CONC_XML        '<UserName>'.
    CONC_XML        ME->AT_AUTH_WS-USERNAME.
    CONC_XML        '</UserName>'.
    CONC_XML        '<Password>'.
    CONC_XML        ME->AT_AUTH_WS-PASSWORD.
    CONC_XML        '</Password>'.
    CONC_XML    '</WsAuthHeader>'.
    CONC_XML '</soap:Header>'.
    CONC_XML '<soap:Body>'.
    CONC_XML    '<ConsultaDescargaFerroviaria xmlns="http://www.unisolution.com.br/">'.
    CONC_XML        '<dataIncial>'.
    V_C_AUX = ME->AT_DT_CHEGADA(4) && '-' && ME->AT_DT_CHEGADA+4(2) && '-' && ME->AT_DT_CHEGADA+6(2).
    CONC_XML                V_C_AUX.
    CONC_XML       '</dataIncial>'.
    CONC_XML       '<dataFinal>'.
    CONC_XML                V_C_AUX.
    CONC_XML       '</dataFinal>'.
    CONC_XML       '<chaveNfe></chaveNfe>'.
    CONC_XML  '</ConsultaDescargaFerroviaria>'.
    CONC_XML    '</soap:Body>'.
    CONC_XML '</soap:Envelope>'.
  ENDMETHOD.


  method PREPARAR_DADOS_GRAVACAO.

    R_OK = ABAP_FALSE.

    LOOP AT ME->AT_DADOS_DESCARGA ASSIGNING FIELD-SYMBOL(<FS_DADOS_DESCARGA>).

      IF <FS_DADOS_DESCARGA>-ID_REGISTRO IS INITIAL.
        "Gerar ID registro
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZREG_L3_WS'
          IMPORTING
            NUMBER                  = <FS_DADOS_DESCARGA>-ID_REGISTRO
          EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            OTHERS                  = 8.

        IF ( SY-SUBRC NE 0 ) OR ( <FS_DADOS_DESCARGA>-ID_REGISTRO IS INITIAL ).
          MESSAGE S028.
          RETURN.
        ENDIF.
      ENDIF.

      LOOP AT <FS_DADOS_DESCARGA>-NOTAS ASSIGNING FIELD-SYMBOL(<FS_DADOS_NF>).
        <FS_DADOS_NF>-ID_REGISTRO = <FS_DADOS_DESCARGA>-ID_REGISTRO.
      ENDLOOP.

      <FS_DADOS_DESCARGA>-SRV_INTEGRACAO = ME->AT_SRV_INTEGRACAO.
      <FS_DADOS_DESCARGA>-DT_REGISTRO = SY-DATUM.
      <FS_DADOS_DESCARGA>-HR_REGISTRO = SY-UZEIT.

    ENDLOOP.

    R_OK = ABAP_TRUE.

  endmethod.


  method SET_DT_CHEGADA.

    ME->AT_DT_CHEGADA = I_DT_CHEGADA.

  endmethod.


  METHOD set_fields.

    TYPES BEGIN OF ty_retorno_msg.
    TYPES message TYPE string.
    TYPES END OF ty_retorno_msg.

    DATA: zcl_token_vli       TYPE REF TO zcl_token_vli,
          lw_data             TYPE string,                         "*-CS2024000593-22.11.2024-JT-#157860-inicio
          lc_texto_vli        TYPE c LENGTH 200,                   "*-CS2024000593-22.11.2024-JT-#157860-inicio
          lw_token            TYPE string,                         "*-CS2024000593-22.11.2024-JT-#157860-inicio
          lc_token_vli        TYPE REF TO zcl_token_vli_new,       "*-CS2024000593-22.11.2024-JT-#157860-inicio
          lc_mensagem         TYPE ty_retorno_msg,
          ex_webservice_token TYPE REF TO zcl_webservice,
          lc_endereco         TYPE string.

    DATA: v_c_aux TYPE string.

    CASE me->at_srv_integracao.
      WHEN '01'. "Terminais VLI

*-CS2024000593-22.11.2024-JT-#157860-inicio
        CREATE OBJECT lc_token_vli.

        TRY .
            lw_token = lc_token_vli->get_token( 'DESCARGA_FERRO_VLI' ).

          CATCH zcx_integracao INTO DATA(ex_integra).
            MESSAGE ID ex_integra->msgid TYPE 'S' NUMBER ex_integra->msgno WITH ex_integra->msgv1 ex_integra->msgv2 ex_integra->msgv3 ex_integra->msgv4 DISPLAY LIKE 'E'.
            RETURN.
          CATCH zcx_error INTO DATA(ex_error).
            MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 DISPLAY LIKE 'E'.
            RETURN.
        ENDTRY.

        lw_data = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2).  "*-CS2024000593-18.11.2024-jt-#146078-inicio

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'access_token'
            value = lw_token.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'client_id'
            value = CONV #( me->at_auth_ws-username ).

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'edi-token'
            value = CONV #( me->at_auth_ws-add01 ).

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'dataDescarga'
            value = lw_data.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'terminal'
            value = CONV #( me->at_sigla_terminal ).

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~request_method'
            value = 'GET'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~server_protocol'
            value = 'HTTP/1.1'.
*-CS2024000593-22.11.2024-JT-#157860-inicio

*-CS2024000593-22.11.2024-JT-#157860-inicio
*        CALL METHOD c_if_http_client->request->set_header_field
*          EXPORTING
*            name  = '~request_method'
*            value = 'GET'.
*
*        CALL METHOD c_if_http_client->request->set_header_field
*          EXPORTING
*            name  = '~server_protocol'
*            value = 'HTTP/1.1'.
*
*        v_c_aux = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2).
*
*        c_if_http_client->request->set_form_field(
*          EXPORTING
*            name  = 'data'
*            value = v_c_aux ).
*
*        c_if_http_client->request->set_form_field(
*          EXPORTING
*            name  = 'terminal'
*            value = CONV #( me->at_sigla_terminal ) ).
*
*        CALL METHOD c_if_http_client->request->set_header_field
*          EXPORTING
*            name  = 'client_id'
*            value = CONV #( me->at_auth_ws-add01 ).
*
*        "Get Access Token
*        FREE zcl_token_vli.
*        CREATE OBJECT zcl_token_vli.
*
*        DATA(_access_token) = zcl_token_vli->get_token( ).
*
*        CALL METHOD c_if_http_client->request->set_header_field
*          EXPORTING
*            name  = 'access_token'
*            value = _access_token.
*-CS2024000593-22.11.2024-JT-#157860-fim

      WHEN '02'. "Terminais BUNGE
        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~request_method'
            value = 'POST'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~server_protocol'
            value = 'HTTP/1.1'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'Content-Type'
            value = 'text/xml; charset=UTF-8'.
      WHEN OTHERS.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~request_method'
            value = 'POST'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = '~server_protocol'
            value = 'HTTP/1.1'.

        CALL METHOD c_if_http_client->request->set_header_field
          EXPORTING
            name  = 'Content-Type'
            value = 'application/xml; charset=UTF-8'.

    ENDCASE.



  ENDMETHOD.


  method SET_SIGLA_TERMINAL.

    ME->AT_SIGLA_TERMINAL = I_SIGLA_TERMINAL.

  endmethod.


  method SET_SRV_INTEGRACAO.

    ME->AT_SRV_INTEGRACAO = I_SRV_INTEGRACAO.

  endmethod.


  METHOD set_url.

    CLEAR: r_url.

    CASE me->at_srv_integracao.
      WHEN '01'. "Terminais VLI

        SELECT SINGLE *
          FROM zauth_webservice INTO me->at_auth_ws
         WHERE service = 'DESCARGA_FERRO_SRV_INT_01'.

        IF sy-subrc EQ 0.
*-CS2024000593-18.11.2024-jt-#146078-inicio
          r_url  = me->at_auth_ws-url.
          DATA(lw_data) = me->at_dt_chegada(4) && '-' && me->at_dt_chegada+4(2) && '-' && me->at_dt_chegada+6(2).  "*-CS2024000593-18.11.2024-jt-#146078-inicio
          r_url = |{ r_url }?dataDescarga={ lw_data }&terminal={ me->at_sigla_terminal }|.
*-CS2024000593-18.11.2024-jt-#146078-fim
        ENDIF.

      WHEN '02'. "Terminais BUNGE
        IF  me->at_sigla_terminal = 'BGESFS'.
          SELECT SINGLE *
            FROM zauth_webservice INTO me->at_auth_ws
           WHERE service = 'DESCARGA_FERRO_SRV_INT_02'.
        ELSE.
          SELECT SINGLE *
            FROM zauth_webservice INTO me->at_auth_ws
           WHERE service = 'DESCARGA_FERRO_SRV_INT_03'.
        ENDIF.

        IF sy-subrc EQ 0.
          r_url  = me->at_auth_ws-url.
        ENDIF.

      WHEN OTHERS.
        MESSAGE s030 WITH me->at_srv_integracao.
        RETURN.
    ENDCASE.

    IF r_url IS INITIAL.
      MESSAGE s031 WITH me->at_srv_integracao.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD tratar_retorno_01.

*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
    DATA: v_node_dados_recebedor TYPE c,
          v_novo_veiculo         TYPE c,
          v_nova_nf              TYPE c.


    DATA: return_code      TYPE i,
          e_resultado      TYPE string,
          v_node_name      TYPE string,
          v_node_value     TYPE string,
          v_vlr_aux        TYPE string,
          v_msg_show       TYPE c LENGTH 200,

          it_zlest0179_tmp TYPE TABLE OF zde_zlest0179_tmp,
          wl_zlest0179_tmp TYPE zde_zlest0179_tmp,

          it_zlest0180_tmp TYPE TABLE OF zde_zlest0180_tmp,
          wl_zlest0180_tmp TYPE zde_zlest0180_tmp,

          wl_zlest0179     TYPE zlest0179,
          wl_zlest0180     TYPE zlest0180,
          t_zlest0180      TYPE zlest0180_t,

          v_cont_reg_tmp   TYPE zlest0179-id_registro,

          v_protocolo_rec  TYPE zlest0179-protocolo_rec,
          v_recebedor_cnpj TYPE zlest0179-recebedor_cnpj,
          v_recebedor_name TYPE zlest0179-recebedor_name,

          lt_dados         TYPE zlese0267,   "*-CS2024000593-22.11.2024-JT-#157860-inicio
          lw_dados         TYPE zlese0268,   "*-CS2024000593-22.11.2024-JT-#157860-inicio
          lw_listanfes     TYPE zlese0273,   "*-CS2024000593-22.11.2024-JT-#157860-inicio
          lw_campos_nfe    TYPE zde_campos_nfe,
          zcl_util         TYPE REF TO zcl_util.

    DATA: js_prop_tab TYPE js_property_tab,
          js_obj      TYPE REF TO cl_java_script.

    DATA: wl_retorno_js TYPE zde_l3_vli_js.

    CLEAR: wl_zlest0179, wl_zlest0180, t_zlest0180[], v_protocolo_rec, v_recebedor_cnpj, v_recebedor_name.

    CREATE OBJECT zcl_util.  "*-CS2024000593-22.11.2024-JT-#157860-inicio

*-CS2024000593-22.11.2024-JT-#157860-inicio
*--------------------------------------------------
*-- carregar dados
*--------------------------------------------------
    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_cdata_retorno
      CHANGING
        data = lt_dados.

    CHECK lt_dados IS NOT INITIAL.

    LOOP AT lt_dados-data INTO lw_dados.
      FREE wl_zlest0179_tmp.

      v_cont_reg_tmp                                = v_cont_reg_tmp + 1.

      lw_campos_nfe                                 = zcl_util->get_atributos_nfe( CONV #( lw_dados-chavecte ) ).

      wl_zlest0179_tmp-id_tmp                       = v_cont_reg_tmp.
      wl_zlest0179_tmp-srv_integracao               = '01' .
      wl_zlest0179_tmp-protocolo_rec                = abap_off.
      wl_zlest0179_tmp-remetente_cnpj               = abap_off.
      wl_zlest0179_tmp-remetente_name               = abap_off.
      wl_zlest0179_tmp-recebedor_cnpj               = abap_off.
      wl_zlest0179_tmp-recebedor_name               = abap_off.
      wl_zlest0179_tmp-sigla_terminal_descarga      = lw_dados-localdescarga-codigo.
      wl_zlest0179_tmp-terminal_descarga            = lw_dados-localdescarga-codigoterminal.
      wl_zlest0179_tmp-ds_terminal_descarga         = lw_dados-localdescarga-codigoterminal.
      wl_zlest0179_tmp-cnpj_terminal_descarga       = lw_dados-localdescarga-cnpjterminal.
      wl_zlest0179_tmp-nm_fantasia_terminal_destino = lw_dados-fluxo-localdestino-codigo.
      wl_zlest0179_tmp-rz_social_terminal_destino   = lw_dados-fluxo-localdestino-codigo.
      wl_zlest0179_tmp-cnpj_terminal_destino        = abap_off.
      wl_zlest0179_tmp-dt_chegada                   = lw_dados-datadescarga(4) && lw_dados-datadescarga+5(2) && lw_dados-datadescarga+8(2).
      wl_zlest0179_tmp-idvagao                      = lw_dados-codigovagao.
      wl_zlest0179_tmp-serie_vagao                  = lw_dados-serievagao.
      wl_zlest0179_tmp-nr_cte                       = lw_campos_nfe-nfnum9.
      wl_zlest0179_tmp-serie_cte                    = lw_campos_nfe-serie.
      wl_zlest0179_tmp-chave_cte                    = lw_dados-chavecte.
      wl_zlest0179_tmp-peso_tara                    = lw_dados-taravagao * 1000.
      wl_zlest0179_tmp-peso_liquido                 = lw_dados-pesocarregamento * 1000.
      wl_zlest0179_tmp-peso_bruto                   = wl_zlest0179_tmp-peso_tara + wl_zlest0179_tmp-peso_liquido.
      wl_zlest0179_tmp-meins                        = 'KG'.
      wl_zlest0179_tmp-matnr                        = abap_off.
      wl_zlest0179_tmp-dt_registro                  = sy-datum.
      wl_zlest0179_tmp-hr_registro                  = sy-uzeit.

      LOOP AT lw_dados-listanfes INTO lw_listanfes.
        FREE wl_zlest0180_tmp.

        lw_campos_nfe                               = zcl_util->get_atributos_nfe( CONV #( lw_listanfes-chave ) ).

        wl_zlest0180_tmp-id_tmp                     = v_cont_reg_tmp.
        wl_zlest0180_tmp-chave_nf                   = lw_listanfes-chave.
        wl_zlest0180_tmp-model                      = lw_campos_nfe-model.
        wl_zlest0180_tmp-nfenum                     = lw_campos_nfe-nfnum9.
        wl_zlest0180_tmp-series                     = lw_campos_nfe-serie.
        wl_zlest0180_tmp-stcd1                      = lw_campos_nfe-stcd1.
        wl_zlest0180_tmp-docdat                     = lw_listanfes-dataemissao(4) && lw_listanfes-dataemissao+5(2) && lw_listanfes-dataemissao+8(2).
        wl_zlest0180_tmp-docnum                     = abap_off.
        wl_zlest0180_tmp-peso_declarado             = lw_listanfes-peso * 1000.
        wl_zlest0180_tmp-peso_rateado               = lw_listanfes-pesovagao * 1000.

        APPEND wl_zlest0180_tmp                    TO wl_zlest0179_tmp-notas.
        APPEND wl_zlest0180_tmp                    TO it_zlest0180_tmp.
      ENDLOOP.

      APPEND wl_zlest0179_tmp                      TO it_zlest0179_tmp.
    ENDLOOP.
*-CS2024000593-22.11.2024-JT-#157860-fim

*-CS2024000593-22.11.2024-JT-#157860-inicio
*    LOOP AT wl_retorno_js-data-vagoesdescarregados-vagao INTO DATA(wl_vagao) .
*
*      CLEAR: wl_zlest0179_tmp.
*
*      "Gerar ID Temporario
*      ADD 1 TO v_cont_reg_tmp.
*
*      wl_zlest0179_tmp-id_tmp                        = v_cont_reg_tmp.
*      wl_zlest0179_tmp-serie_vagao                   = wl_vagao-serievagao.
*      wl_zlest0179_tmp-idvagao                       = wl_vagao-codigovagao.
*
*      SHIFT wl_zlest0179_tmp-idvagao  LEFT DELETING LEADING '0'.
*
*      wl_zlest0179_tmp-sigla_terminal_descarga       = wl_vagao-codigolocaldescarga.
*      wl_zlest0179_tmp-terminal_descarga             = wl_vagao-descricaolocaldescarga.
*      wl_zlest0179_tmp-ds_terminal_descarga          = wl_vagao-descricaolocaldescarga.
*      wl_zlest0179_tmp-nm_fantasia_terminal_destino  = wl_vagao-descricaoterminaldestino.
*      wl_zlest0179_tmp-rz_social_terminal_destino    = wl_vagao-descricaoterminaldestino.
*
*      me->conversion_cnpj_input( CHANGING i_cnpj = wl_vagao-cnpjterminaldestino ).
*      wl_zlest0179_tmp-cnpj_terminal_destino = wl_vagao-cnpjterminaldestino.
*
*      v_vlr_aux = wl_vagao-datadescarga.
*      REPLACE ALL OCCURRENCES OF '-' IN v_vlr_aux WITH space.
*      wl_zlest0179_tmp-dt_chegada = v_vlr_aux.
*
*      v_vlr_aux = wl_vagao-pesobruto.
*      REPLACE ALL OCCURRENCES OF ',' IN v_vlr_aux WITH '.'.
*      CONDENSE v_vlr_aux NO-GAPS.
*      wl_zlest0179_tmp-peso_bruto = v_vlr_aux.
*
*      v_vlr_aux = wl_vagao-pesotara.
*      REPLACE ALL OCCURRENCES OF ',' IN v_vlr_aux WITH '.'.
*      CONDENSE v_vlr_aux NO-GAPS.
*      wl_zlest0179_tmp-peso_tara = v_vlr_aux.
*
*      v_vlr_aux = wl_vagao-pesoliquido.
*      REPLACE ALL OCCURRENCES OF ',' IN v_vlr_aux WITH '.'.
*      CONDENSE v_vlr_aux NO-GAPS.
*      wl_zlest0179_tmp-peso_liquido = v_vlr_aux.
*
*      wl_zlest0179_tmp-meins  = wl_vagao-unidademedida.
*
*      wl_zlest0179_tmp-chave_cte = wl_vagao-chavecte.
*      wl_zlest0179_tmp-nr_cte    = wl_vagao-chavecte+25(9).
*      wl_zlest0179_tmp-serie_cte = wl_vagao-chavecte+22(3).
*
*      LOOP AT wl_vagao-nfe-listanfes INTO DATA(_wl_liesta_nfes).
*        CLEAR: wl_zlest0180_tmp.
*
*        wl_zlest0180_tmp-id_tmp = v_cont_reg_tmp.
*
*
*        v_vlr_aux = _wl_liesta_nfes-dataemissao.
*        REPLACE ALL OCCURRENCES OF '-' IN v_vlr_aux WITH space.
*        wl_zlest0180_tmp-docdat = v_vlr_aux.
*
*        wl_zlest0180_tmp-chave_nf = _wl_liesta_nfes-chavenfe.
*        wl_zlest0180_tmp-model    = '55'.
*        wl_zlest0180_tmp-stcd1    = wl_zlest0180_tmp-chave_nf+6(14).
*        wl_zlest0180_tmp-nfenum   = wl_zlest0180_tmp-chave_nf+25(9).
*        wl_zlest0180_tmp-series   = wl_zlest0180_tmp-chave_nf+22(3).
*
*        v_vlr_aux = _wl_liesta_nfes-pesodeclarado.
*        REPLACE ALL OCCURRENCES OF ',' IN v_vlr_aux WITH '.'.
*        CONDENSE v_vlr_aux NO-GAPS.
*        wl_zlest0180_tmp-peso_declarado = v_vlr_aux.
*
*        v_vlr_aux = _wl_liesta_nfes-pesorateado.
*        REPLACE ALL OCCURRENCES OF ',' IN v_vlr_aux WITH '.'.
*        CONDENSE v_vlr_aux NO-GAPS.
*        wl_zlest0180_tmp-peso_rateado = v_vlr_aux.
*
*        APPEND wl_zlest0180_tmp TO wl_zlest0179_tmp-notas.
*
*      ENDLOOP.
*
*      CASE wl_zlest0179_tmp-meins.
*        WHEN 'TO'.
*          wl_zlest0179_tmp-peso_bruto    = wl_zlest0179_tmp-peso_bruto    * 1000.
*          wl_zlest0179_tmp-peso_tara     = wl_zlest0179_tmp-peso_tara     * 1000.
*          wl_zlest0179_tmp-peso_liquido  = wl_zlest0179_tmp-peso_liquido  * 1000.
*
*          LOOP AT wl_zlest0179_tmp-notas ASSIGNING FIELD-SYMBOL(<fs_nota>).
*            <fs_nota>-peso_declarado = <fs_nota>-peso_declarado  * 1000.
*            <fs_nota>-peso_rateado   = <fs_nota>-peso_rateado    * 1000.
*          ENDLOOP.
*
*          wl_zlest0179_tmp-meins = 'KG'.
*        WHEN OTHERS.
*      ENDCASE.
*
*      APPEND wl_zlest0179_tmp TO it_zlest0179_tmp.
*
*    ENDLOOP.

    me->at_dados_descarga[] = it_zlest0179_tmp[].

  ENDMETHOD.


    METHOD TRATAR_RETORNO_02.


*----------------------------------------------------------------*
*   Variaveis Controle XML
*----------------------------------------------------------------*
      DATA: V_NODE_DADOS_RECEBEDOR TYPE C,
            V_NOVO_VEICULO         TYPE C,
            V_NOVA_NF              TYPE C.


      DATA: RETURN_CODE      TYPE I,
            E_RESULTADO      TYPE STRING,
            V_NODE_NAME      TYPE STRING,
            V_NODE_VALUE     TYPE STRING,
            V_VLR_AUX        TYPE STRING,
            V_MSG_SHOW       TYPE C LENGTH 200,

            IT_ZLEST0179_TMP TYPE TABLE OF ZDE_ZLEST0179_TMP,
            WL_ZLEST0179_TMP TYPE ZDE_ZLEST0179_TMP,

            IT_ZLEST0180_TMP TYPE TABLE OF ZDE_ZLEST0180_TMP,
            WL_ZLEST0180_TMP TYPE ZDE_ZLEST0180_TMP,

            WL_ZLEST0179     TYPE ZLEST0179,
            WL_ZLEST0180     TYPE ZLEST0180,
            T_ZLEST0180      TYPE ZLEST0180_T,

            V_CONT_REG_TMP   TYPE ZLEST0179-ID_REGISTRO,

            V_PROTOCOLO_REC  TYPE ZLEST0179-PROTOCOLO_REC,
            V_RECEBEDOR_CNPJ TYPE ZLEST0179-RECEBEDOR_CNPJ,
            V_RECEBEDOR_NAME TYPE ZLEST0179-RECEBEDOR_NAME.

      DATA: JS_PROP_TAB TYPE JS_PROPERTY_TAB,
            JS_OBJ      TYPE REF TO CL_JAVA_SCRIPT.

      DATA: WL_RETORNO_JS TYPE ZDE_L3_VLI_JS.


      CLEAR: WL_ZLEST0179, WL_ZLEST0180, T_ZLEST0180[], V_PROTOCOLO_REC, V_RECEBEDOR_CNPJ, V_RECEBEDOR_NAME.

      "XML
      DATA(_XML_FERR_DESCARREGADOS) = C_XML_RETURN->FIND_NODE( EXPORTING NAME = 'ConsultaDescargaFerroviariaResult' ).

      IF _XML_FERR_DESCARREGADOS IS NOT INITIAL.

        DATA(_ITERATOR_FERR_DESCAR) = _XML_FERR_DESCARREGADOS->CREATE_ITERATOR( ).
        DATA(_XML_NODE_FERR_DESCAR) = _ITERATOR_FERR_DESCAR->GET_NEXT( ).

        WHILE _XML_NODE_FERR_DESCAR IS NOT INITIAL.

          CASE _XML_NODE_FERR_DESCAR->GET_TYPE( ).
            WHEN: IF_IXML_NODE=>CO_NODE_ELEMENT.

              DATA(_NODE_NAME)  = _XML_NODE_FERR_DESCAR->GET_NAME( ).
              DATA(_NODE_VALOR) = _XML_NODE_FERR_DESCAR->GET_VALUE( ).

              CASE _NODE_NAME.
                WHEN 'PlacaVagao'.
                  "Novo
                  IF WL_ZLEST0179_TMP IS NOT INITIAL.
                    APPEND WL_ZLEST0180_TMP TO WL_ZLEST0179_TMP-NOTAS.
                    APPEND WL_ZLEST0179_TMP TO IT_ZLEST0179_TMP.
                  ENDIF.
                  CLEAR: WL_ZLEST0179_TMP.
                  CLEAR: WL_ZLEST0180_TMP.

                  "Gerar ID Temporario
                  ADD 1 TO V_CONT_REG_TMP.

                  WL_ZLEST0179_TMP-ID_TMP                        = V_CONT_REG_TMP.
                  WL_ZLEST0179_TMP-SERIE_VAGAO                   = _NODE_VALOR(3).
                  DATA(LEN) = STRLEN( _NODE_VALOR ) - 3.
                  WL_ZLEST0179_TMP-IDVAGAO                       = _NODE_VALOR+3(LEN).

                  SHIFT WL_ZLEST0179_TMP-IDVAGAO  LEFT DELETING LEADING '0'.

                  WL_ZLEST0179_TMP-SIGLA_TERMINAL_DESCARGA       = ME->AT_SIGLA_TERMINAL.
                  "
                  IF ME->AT_SIGLA_TERMINAL = 'BGESFS'.
                    WL_ZLEST0179_TMP-TERMINAL_DESCARGA             = 'BUNGE ALIMENTOS S.A.'.
                    WL_ZLEST0179_TMP-DS_TERMINAL_DESCARGA          = 'São Francisco do Sul'.
                    WL_ZLEST0179_TMP-CNPJ_TERMINAL_DESCARGA        = '84046101000940'.
                    "
                    WL_ZLEST0179_TMP-CNPJ_TERMINAL_DESTINO         = '84046101000940'.
                    WL_ZLEST0179_TMP-NM_FANTASIA_TERMINAL_DESTINO  = 'São Francisco do Sul'.
                    WL_ZLEST0179_TMP-RZ_SOCIAL_TERMINAL_DESTINO    = 'BUNGE ALIMENTOS S.A.'.
                  ELSE.
                    WL_ZLEST0179_TMP-TERMINAL_DESCARGA             = 'BUNGE ALIMENTOS S.A.'.
                    WL_ZLEST0179_TMP-DS_TERMINAL_DESCARGA          = 'Paranagua'.
                    WL_ZLEST0179_TMP-CNPJ_TERMINAL_DESCARGA        = '84046101028101'.
                    "
                    WL_ZLEST0179_TMP-CNPJ_TERMINAL_DESTINO         = '84046101028101'.
                    WL_ZLEST0179_TMP-NM_FANTASIA_TERMINAL_DESTINO  = 'Paranagua'.
                    WL_ZLEST0179_TMP-RZ_SOCIAL_TERMINAL_DESTINO    = 'BUNGE ALIMENTOS S.A.'.
                  ENDIF.



                  WL_ZLEST0180_TMP-ID_TMP = V_CONT_REG_TMP.
                WHEN 'CnpjCpfFornecedor'.
                  "
                  ME->CONVERSION_CNPJ_INPUT( CHANGING I_CNPJ = _NODE_VALOR ).
                  WL_ZLEST0179_TMP-REMETENTE_CNPJ         = _NODE_VALOR.
                  SELECT SINGLE NAME1
                    INTO  WL_ZLEST0179_TMP-REMETENTE_NAME
                    FROM LFA1
                   WHERE STCD1 =  WL_ZLEST0179_TMP-REMETENTE_CNPJ .
                  "
                WHEN 'DataEmissaoNF'.
                  V_VLR_AUX = _NODE_VALOR.
                  REPLACE ALL OCCURRENCES OF '-' IN V_VLR_AUX WITH SPACE.
                  WL_ZLEST0180_TMP-DOCDAT = V_VLR_AUX.
                WHEN 'PesoDeclarado'.
                  V_VLR_AUX = _NODE_VALOR.
                  REPLACE ALL OCCURRENCES OF ',' IN V_VLR_AUX WITH '.'.
                  CONDENSE V_VLR_AUX NO-GAPS.
                  WL_ZLEST0180_TMP-PESO_DECLARADO = V_VLR_AUX.
                WHEN 'PesoRateado'.
                  V_VLR_AUX = _NODE_VALOR.
                  REPLACE ALL OCCURRENCES OF ',' IN V_VLR_AUX WITH '.'.
                  CONDENSE V_VLR_AUX NO-GAPS.
                  WL_ZLEST0180_TMP-PESO_RATEADO = V_VLR_AUX.
                WHEN 'DataDescarga'.
                  V_VLR_AUX = _NODE_VALOR.
                  REPLACE ALL OCCURRENCES OF '-' IN V_VLR_AUX WITH SPACE.
                  WL_ZLEST0179_TMP-DT_CHEGADA = V_VLR_AUX.
                WHEN 'PesoBruto'.
                  V_VLR_AUX = _NODE_VALOR.
                  REPLACE ALL OCCURRENCES OF ',' IN V_VLR_AUX WITH '.'.
                  CONDENSE V_VLR_AUX NO-GAPS.
                  WL_ZLEST0179_TMP-PESO_BRUTO = V_VLR_AUX.
                WHEN 'PesoTara'.
                  V_VLR_AUX = _NODE_VALOR.
                  REPLACE ALL OCCURRENCES OF ',' IN V_VLR_AUX WITH '.'.
                  CONDENSE V_VLR_AUX NO-GAPS.
                  WL_ZLEST0179_TMP-PESO_TARA = V_VLR_AUX.
                WHEN 'PesoLiquido'.
                  V_VLR_AUX = _NODE_VALOR.
                  REPLACE ALL OCCURRENCES OF ',' IN V_VLR_AUX WITH '.'.
                  CONDENSE V_VLR_AUX NO-GAPS.
                  WL_ZLEST0179_TMP-PESO_LIQUIDO = V_VLR_AUX.
                WHEN 'UnidadeMedida'.
                  WL_ZLEST0179_TMP-MEINS  = _NODE_VALOR.
                  CASE WL_ZLEST0179_TMP-MEINS.
                    WHEN 'TO'.
                      WL_ZLEST0179_TMP-PESO_BRUTO    = WL_ZLEST0179_TMP-PESO_BRUTO    * 1000.
                      WL_ZLEST0179_TMP-PESO_TARA     = WL_ZLEST0179_TMP-PESO_TARA     * 1000.
                      WL_ZLEST0179_TMP-PESO_LIQUIDO  = WL_ZLEST0179_TMP-PESO_LIQUIDO  * 1000.

                      LOOP AT WL_ZLEST0179_TMP-NOTAS ASSIGNING FIELD-SYMBOL(<FS_NOTA>).
                        <FS_NOTA>-PESO_DECLARADO = <FS_NOTA>-PESO_DECLARADO  * 1000.
                        <FS_NOTA>-PESO_RATEADO   = <FS_NOTA>-PESO_RATEADO    * 1000.
                      ENDLOOP.

                      WL_ZLEST0179_TMP-MEINS = 'KG'.
                    WHEN OTHERS.
                  ENDCASE.
                WHEN 'ChaveNFe'.

                  WL_ZLEST0179_TMP-CHAVE_CTE = ' '.
                  WL_ZLEST0179_TMP-NR_CTE    = '999999999'.
                  WL_ZLEST0179_TMP-SERIE_CTE = '999'.

                  "
                  WL_ZLEST0180_TMP-CHAVE_NF  = _NODE_VALOR.
                  WL_ZLEST0180_TMP-STCD1    = WL_ZLEST0180_TMP-CHAVE_NF+6(14).
                  WL_ZLEST0180_TMP-NFENUM   = WL_ZLEST0180_TMP-CHAVE_NF+25(9).
                  WL_ZLEST0180_TMP-SERIES   = WL_ZLEST0180_TMP-CHAVE_NF+22(3).
                  WL_ZLEST0180_TMP-MODEL    = '55'.
                WHEN 'NumeroCTe'.
*                  WL_ZLEST0179_TMP-NR_CTE = _NODE_VALOR.
              ENDCASE.
          ENDCASE.

          _XML_NODE_FERR_DESCAR = _ITERATOR_FERR_DESCAR->GET_NEXT( ).
        ENDWHILE.

      ENDIF.

      IF WL_ZLEST0179_TMP IS NOT INITIAL.
        APPEND WL_ZLEST0180_TMP TO WL_ZLEST0179_TMP-NOTAS.
        APPEND WL_ZLEST0179_TMP TO IT_ZLEST0179_TMP.
      ENDIF.

      ME->AT_DADOS_DESCARGA[] = IT_ZLEST0179_TMP[].

    ENDMETHOD.


  METHOD TRATAR_RETORNO_CONSULTA.

    CASE ME->AT_SRV_INTEGRACAO.
      WHEN '01'. "Terminais VLI

        ME->TRATAR_RETORNO_01(
          EXPORTING
            I_CDATA_RETORNO = I_CDATA_RETORNO
          CHANGING
            C_XML_RETURN    = C_XML_RETURN
        ).

      WHEN '02'. "Terminais BUNGE

        ME->TRATAR_RETORNO_02(
          EXPORTING
            I_CDATA_RETORNO = I_CDATA_RETORNO
          CHANGING
            C_XML_RETURN    = C_XML_RETURN
        ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  method TRATAR_XML.

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN C_XML WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN C_XML WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN C_XML WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN C_XML WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN C_XML WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN C_XML WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN C_XML WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN C_XML WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN C_XML WITH 'o' IGNORING CASE.

  endmethod.
ENDCLASS.
