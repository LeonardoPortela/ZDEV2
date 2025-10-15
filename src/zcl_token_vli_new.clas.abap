class ZCL_TOKEN_VLI_NEW definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .

  data AT_WEBSERVICE type ZAUTH_WEBSERVICE .
  data AT_SERVICO type STRING .
  data AT_TOKEN type STRING .

  methods GET_TOKEN
    importing
      !I_SERVICO type STRING
    returning
      value(E_TOKEN) type STRING
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_DS_URL
    importing
      !I_SERVICO type STRING
    returning
      value(R_TOKEN_VLI_NEW) type ref to ZCL_TOKEN_VLI_NEW
    raising
      ZCX_INTEGRACAO .
  methods SET_DS_DATA
    importing
      !I_INTEGRACAO type ZINTEGRACAO
    returning
      value(R_TOKEN_VLI_NEW) type ref to ZCL_TOKEN_VLI_NEW .
  methods SET_SEND_MSG
    exporting
      !E_ID_INTEGRACAO type ZDE_ID_INTEGRACAO
      !E_INTEGRACAO type ZINTEGRACAO
      value(E_HEADER_FIELDS) type ZDE_HEADER_FIELD_T
    returning
      value(R_TOKEN_VLI_NEW) type ref to ZCL_TOKEN_VLI_NEW
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TOKEN_VLI_NEW IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = '245'.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'TERMINAL_VLI'.

  ENDMETHOD.


  METHOD get_token.

    TYPES: BEGIN OF ty_retorno,
             access_token TYPE string,
             expires_in   TYPE string,
             token_type   TYPE string.
    TYPES: END OF ty_retorno.

    DATA: lc_integracao TYPE zintegracao,
          lc_retorno    TYPE ty_retorno,
          l_error       TYPE c,
          lv_token      TYPE zintegracao0001.

    FREE:  me->zif_integracao_inject~at_header_fields,
           me->zif_integracao_inject~at_info_request_http,
           e_token.

    me->zif_integracao_inject~at_tp_integracao              = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_referencia-tp_referencia   = 'TOKEN'.
    me->zif_integracao_inject~at_referencia-id_referencia   = 'TOKEN_' && i_servico.

    DATA(lit_header_auth) = zcl_gestao_token=>get_token_valido( i_id_token = '0006' ).

    IF lit_header_auth[] IS NOT INITIAL.
      READ TABLE lit_header_auth INTO DATA(was_header_auth) INDEX 1.
      SPLIT was_header_auth-value AT abap_off INTO DATA(lc_constant) e_token.
      RETURN.
    ENDIF.

    TRY.
        me->set_ds_url(   EXPORTING i_servico       = i_servico
         )->set_ds_data(  EXPORTING i_integracao    = lc_integracao
         )->set_send_msg( IMPORTING e_id_integracao = DATA(lc_id_integracao)
                                    e_integracao    = lc_integracao
         ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_integra->msgid
                              msgno = ex_integra->msgno
                              attr1 = CONV #( ex_integra->msgv1 )
                              attr2 = CONV #( ex_integra->msgv2 )
                              attr3 = CONV #( ex_integra->msgv3 )
                              attr4 = CONV #( ex_integra->msgv4 ) )
            msgid  = ex_integra->msgid
            msgno  = ex_integra->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_integra->msgv1 )
            msgv2  = CONV #( ex_integra->msgv2 )
            msgv3  = CONV #( ex_integra->msgv3 )
            msgv4  = CONV #( ex_integra->msgv4 ).

      CATCH zcx_error      INTO DATA(ex_error).    "  "
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING
            textid = VALUE #( msgid = ex_error->msgid
                              msgno = ex_error->msgno
                              attr1 = CONV #( ex_error->msgv1 )
                              attr2 = CONV #( ex_error->msgv2 )
                              attr3 = CONV #( ex_error->msgv3 )
                              attr4 = CONV #( ex_error->msgv4 ) )
            msgid  = ex_error->msgid
            msgno  = ex_error->msgno
            msgty  = 'E'
            msgv1  = CONV #( ex_error->msgv1 )
            msgv2  = CONV #( ex_error->msgv2 )
            msgv3  = CONV #( ex_error->msgv3 )
            msgv4  = CONV #( ex_error->msgv4 ).
    ENDTRY.

*---------------------------------------
*---avalia retorno JSON
*---------------------------------------
    CASE i_servico.

      WHEN 'DESCARGA_RODOV_VLI' OR
           'CARGA_FERROVIA_VLI' OR
           'DESCARGA_FERRO_VLI'.
        /ui2/cl_json=>deserialize( EXPORTING json = lc_integracao-ds_data_retorno
                                   CHANGING  data = lc_retorno ).
        e_token = lc_retorno-access_token.

        lv_token-id_token     = '0006'.
        lv_token-access_token = lc_retorno-access_token.
        lv_token-token_type   = 'Basic'.
        lv_token-expires_in   = lc_retorno-expires_in.

        zcl_gestao_token=>update_token( i_token = lv_token ).
    ENDCASE.

  ENDMETHOD.


  METHOD set_ds_data.

    DATA: lv_var_string TYPE string,
          lv_string     TYPE string,
          lv_xstring    TYPE xstring.

    r_token_vli_new = me.

    FREE: me->zif_integracao_inject~at_header_fields.

    CASE me->at_servico.

      WHEN 'DESCARGA_RODOV_VLI' OR
           'CARGA_FERROVIA_VLI' OR
           'DESCARGA_FERRO_VLI'.
        lv_var_string = |{ me->at_webservice-username }:{ me->at_webservice-password }|.

        "Convert base64.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = lv_var_string
          IMPORTING
            buffer = lv_xstring
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

        CHECK sy-subrc = 0.

        "Converter XSTRING em BASE64.
        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = lv_xstring
          IMPORTING
            output = lv_string.

        CHECK lv_string IS NOT INITIAL.

        me->zif_integracao_inject~at_header_fields =
          VALUE #(
                 ( name  = 'Content-Type'      value = 'application/x-www-form-urlencoded' )
                 ( name  = 'Authorization'     value = |Basic { lv_string }| )
                 ).

        me->zif_integracao_inject~at_form_fields =
          VALUE #(
                 ( name = 'grant_type'         value = 'client_credentials' )
                 ).

        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.
*       me->zif_integracao_inject~at_info_request_http-ds_body         = l_json.
        me->zif_integracao_inject~at_info_request_http-ds_metodo       = 'POST'.

    ENDCASE.

  ENDMETHOD.


  METHOD set_ds_url.

    DATA: l_url     TYPE string,
          l_servico TYPE string.

    r_token_vli_new = me.

    CASE i_servico.
      WHEN 'DESCARGA_RODOV_VLI' OR
           'CARGA_FERROVIA_VLI' OR
           'DESCARGA_FERRO_VLI'.
        l_servico = 'VLI_L1_L2_L3_TOKEN'.
    ENDCASE.

    SELECT SINGLE *
             FROM zauth_webservice
             INTO @DATA(wa_webservice)
            WHERE service = @l_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'T'
                            attr2 = 'TC' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'T'
          msgv2  = 'TC'.
    ENDIF.

    me->at_webservice = wa_webservice.
    me->at_servico    = i_servico.

    CASE i_servico.
      WHEN 'DESCARGA_RODOV_VLI' OR
           'CARGA_FERROVIA_VLI' OR
           'DESCARGA_FERRO_VLI'.
        l_url = wa_webservice-url.
    ENDCASE.

    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = wa_webservice-content_type.
*   me->zif_integracao_inject~at_info_request_http-ds_url_token        = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url              = l_url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.

  ENDMETHOD.


  METHOD set_send_msg.

    DATA: lc_integrar    TYPE REF TO zcl_integracao.

    r_token_vli_new = me.

    CREATE OBJECT lc_integrar.

    lc_integrar->zif_integracao~at_form_fields = me->zif_integracao_inject~at_form_fields.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING  e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao    = e_integracao
      )->free(
      ).

  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
