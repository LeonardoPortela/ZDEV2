FUNCTION zsdmf011_busca_compra_sigam.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SAFRA) TYPE  AJAHR OPTIONAL
*"     REFERENCE(I_ID_COMPRA) TYPE  ZDE_ID_COMPRA_SG OPTIONAL
*"     REFERENCE(I_CPF) TYPE  STCD2 OPTIONAL
*"     REFERENCE(I_CNPJ) TYPE  STCD1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_ZSDT0260) TYPE  ZSDT0260_T
*"     REFERENCE(E_ERRO) TYPE  CHAR1
*"----------------------------------------------------------------------


  DATA: http_client     TYPE REF TO if_http_client,
        return_code     TYPE i,
        v_url           TYPE ui_src_url,
        v_service       TYPE /ui2/service_name,

        v_cdata_retorno TYPE string,

        it_retorno_js   TYPE TABLE OF zde_json_sigam_compra,
        ws_retorno_js   TYPE          zde_json_sigam_compra,

        _auth_service   TYPE zauth_webservice,
        it_0260         TYPE TABLE OF zsdt0260,
        ws_0260         TYPE zsdt0260.

  "Seleciona na tabela de cadastro de WebService para verificar se aquele serviço existe.
  IF i_safra IS NOT INITIAL.
    SELECT SINGLE *
      FROM zauth_webservice INTO _auth_service
      WHERE service = 'SIGAM_COMPRA_SAFRA'.
  ENDIF.

  IF i_id_compra IS NOT INITIAL.
    SELECT SINGLE *
      FROM zauth_webservice INTO _auth_service
      WHERE service = 'SIGAM_COMPRA_ID'.
  ENDIF.

  IF ( sy-subrc NE 0 ) OR ( _auth_service-url IS INITIAL ).
    MESSAGE i012(ztoken_siscomex) DISPLAY LIKE 'E'.
    e_erro = abap_true. EXIT.
  ELSE.
    v_url = _auth_service-url.
  ENDIF.

  IF i_cpf IS NOT INITIAL.
    CONCATENATE v_url i_cpf INTO v_url.
  ENDIF.

  IF i_cnpj IS NOT INITIAL.
    CONCATENATE v_url i_cnpj INTO v_url.
  ENDIF.

  IF i_safra IS NOT INITIAL.
    CONCATENATE v_url '/' i_safra INTO v_url.
  ENDIF.

  IF i_id_compra IS NOT INITIAL.
    CONCATENATE v_url '/' i_id_compra INTO v_url.
  ENDIF.

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

  zcl_webservice=>zif_webservice~add_token_opus_http_cliente(
    EXPORTING
      i_url_destino              = CONV #( v_url )
      i_url_token                = CONV #( _auth_service-url_token )
    CHANGING
      i_http                     = http_client
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      OTHERS                     = 5  ).

  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = 'GET'.


  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = '~server_protocol'
      value = 'HTTP/1.1'.


  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4.

  CASE sy-subrc.
    WHEN 1.
      http_client->close( ).
      MESSAGE i008(ztoken_siscomex) WITH | { v_url } (Send) | DISPLAY LIKE 'E'. "RAISING HTTP_COMMUNICATION_FAILURE.
      e_erro = abap_true. EXIT.
    WHEN 2.
      http_client->close( ).
      MESSAGE i009(ztoken_siscomex) WITH | { v_url } (Send) | DISPLAY LIKE 'E'. "RAISING HTTP_INVALID_STATE.
      e_erro = abap_true. EXIT.
    WHEN 3.
      http_client->close( ).
      MESSAGE i010(ztoken_siscomex) WITH | { v_url } (Send) | DISPLAY LIKE 'E'. "RAISING HTTP_PROCESSING_FAILED.
      e_erro = abap_true. EXIT.
    WHEN 4.
      http_client->close( ).
      MESSAGE i011(ztoken_siscomex) WITH | { v_url } (Send) | DISPLAY LIKE 'E'. "RAISING HTTP_INVALID_TIMEOUT.
      e_erro = abap_true. EXIT.
  ENDCASE.

  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3.

  CASE sy-subrc.
    WHEN 1.
      http_client->close( ).
      MESSAGE i008(ztoken_siscomex) WITH | { v_url } (Receive) |  DISPLAY LIKE 'E'. "RAISING HTTP_COMMUNICATION_FAILURE.
      e_erro = abap_true. EXIT.
    WHEN 2.
      http_client->close( ).
      MESSAGE i009(ztoken_siscomex) WITH | { v_url } (Receive) |  DISPLAY LIKE 'E'. "RAISING HTTP_INVALID_STATE.
      e_erro = abap_true. EXIT.
    WHEN 3.
      http_client->close( ).
      MESSAGE i010(ztoken_siscomex) WITH | { v_url } (Receive) |  DISPLAY LIKE 'E'. "RAISING HTTP_PROCESSING_FAILED.
      e_erro = abap_true. EXIT.
  ENDCASE.

  "Check return content
  http_client->response->get_status( IMPORTING code = return_code ).

  v_cdata_retorno = http_client->response->get_cdata( ).

  http_client->close( ).

  " Trata Retorno - ERROS
  CASE RETURN_CODE.
   WHEN 200.
    e_erro = abap_false.
   WHEN 403.
    MESSAGE i000(ztoken_siscomex) with '(WS)-Requisição não autorizada!' DISPLAY LIKE 'E'.
    e_erro = abap_true. EXIT.
   WHEN 404.
    MESSAGE i000(ztoken_siscomex) with '(WS)-Registro não encontrado!' DISPLAY LIKE 'E'.
    e_erro = abap_true. EXIT.
   WHEN OTHERS.
    MESSAGE i000(ztoken_siscomex) with '(WS)-Consulta não realizada.  RETURN_CODE. = '  RETURN_CODE  DISPLAY LIKE 'E'.
    e_erro = abap_true. EXIT.
  ENDCASE.


  "Trata retorno JSON - OK
  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = v_cdata_retorno
    CHANGING
      data = it_retorno_js.

  "Preenche tabela de Retorno

  REFRESH: it_0260,
           e_zsdt0260.
  CLEAR ws_0260.

  LOOP AT it_retorno_js INTO ws_retorno_js.

    ws_0260-id_compra          = ws_retorno_js-idcompra.
    ws_0260-safra              = ws_retorno_js-safra.
    ws_0260-status             = ws_retorno_js-status.
    ws_0260-id_filial_sap      = ws_retorno_js-codigoempresafilialsap.
    ws_0260-id_material_sap    = ws_retorno_js-codmaterialsap.
    ws_0260-id_cliente_sap     = ws_retorno_js-codclientesap.
    ws_0260-id_fornec_sap      = ws_retorno_js-codfornecedorsap.
    ws_0260-compra_fim_export  = ws_retorno_js-inmemorandoexportacao.
    CONCATENATE ws_retorno_js-datacompra+6(04)
                ws_retorno_js-datacompra+3(02)
                ws_retorno_js-datacompra(02) INTO ws_0260-data_compra.

    APPEND ws_0260 TO it_0260.

  ENDLOOP.

  e_zsdt0260 = it_0260.

ENDFUNCTION.
