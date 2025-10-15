class ZCL_INT_OB_TOKEN_TRACE_COTTON definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  types:
    BEGIN OF ty_token,
             tipo TYPE char01,
           END OF ty_token .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '209' ##NO_TEXT.
  data AT_DADOS_BODY type ZPPS0005 .
  data AT_TOKEN type ZPPS0015 .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
private section.

  data AT_PARAM_WEBSERVICE type ZAUTH_WEBSERVICE .
ENDCLASS.



CLASS ZCL_INT_OB_TOKEN_TRACE_COTTON IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    r_if_integracao_inject = me.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_integracao_outbound~build_info_request.

    DATA: lwa_token TYPE zpps0015.

    r_if_integracao_outbound = me.

    CLEAR: me->at_token.

    IF i_info_request IS NOT INITIAL.
      MOVE-CORRESPONDING i_info_request TO lwa_token.
    ENDIF.

    me->at_token = lwa_token.

    IF me->at_token-tipo IS INITIAL.
      me->at_token-tipo = '1'.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~EXECUTE_REQUEST.

    R_IF_INTEGRACAO_OUTBOUND = ME.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST( I_INFO_REQUEST = I_INFO_REQUEST
      )->GET_DATA( IMPORTING E_DATA = DATA(LC_DATA)
      )->SET_DATA( EXPORTING I_DATA = LC_DATA
      )->SET_URL(
      )->SET_ID_REFERENCIA(
      )->SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO E_INTEGRACAO	= E_INTEGRACAO
      ).

  endmethod.


  METHOD zif_integracao_outbound~get_data.

    r_if_integracao_outbound = me.

    CLEAR: e_data.


    CASE me->at_token-tipo.
      WHEN '1'.

        SELECT SINGLE *
          FROM zauth_webservice INTO me->zif_integracao_outbound~at_auth_webservice
         WHERE service = 'ENVIA_TRACE_TOKEN'.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_integracao
            EXPORTING
              textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                                msgno = zcx_integracao=>zcx_erro_geral-msgno
                                attr1 = 'Serviço não configurado: '
                                attr2 = 'ENVIA_TRACE_TOKEN' )
              msgid  = zcx_integracao=>zcx_erro_geral-msgid
              msgno  = zcx_integracao=>zcx_erro_geral-msgno
              msgty  = 'E'
              msgv1  = 'Serviço não configurado: '
              msgv2  = 'ENVIA_TRACE_TOKEN'.
        ENDIF.


        e_data = '{ "username": '.
        CONCATENATE e_data '"' me->zif_integracao_outbound~at_auth_webservice-username '","password": "' me->zif_integracao_outbound~at_auth_webservice-password '"}' INTO e_data.

      WHEN '2'.

        SELECT SINGLE *
          FROM zauth_webservice INTO me->zif_integracao_outbound~at_auth_webservice
         WHERE service = 'TRACE_COTTON_TOKEN'.

        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_integracao
            EXPORTING
              textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                                msgno = zcx_integracao=>zcx_erro_geral-msgno
                                attr1 = 'Serviço não configurado: '
                                attr2 = 'TRACE_COTTON_TOKEN' )
              msgid  = zcx_integracao=>zcx_erro_geral-msgid
              msgno  = zcx_integracao=>zcx_erro_geral-msgno
              msgty  = 'E'
              msgv1  = 'Serviço não configurado: '
              msgv2  = 'TRACE_COTTON_TOKEN'.
        ENDIF.

        e_data = 'grant_type='     && me->zif_integracao_outbound~at_auth_webservice-username &&
                 '&client_id='     && me->zif_integracao_outbound~at_auth_webservice-password &&
                 '&client_secret=' && me->zif_integracao_outbound~at_auth_webservice-add01    &&
                 '&resource='      && me->zif_integracao_outbound~at_auth_webservice-add02.

    ENDCASE.



  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_token_trace_cotton.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    types: BEGIN OF TY_OAUTH,
            LOGIN        TYPE STRING,
            ACCESS_TOKEN TYPE STRING,
          END OF TY_OAUTH .

    DATA: lva_token TYPE string.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO,
          lc_retorno  TYPE TY_OAUTH.


    R_IF_INTEGRACAO_OUTBOUND = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.
    FREE: lc_integrar.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

    CLEAR: me->zif_integracao_inject~at_header_fields.

    lva_token = |Bearer { lc_retorno-access_token } |.

    APPEND VALUE #( name = 'Authorization'  value = lva_token ) TO me->zif_integracao_inject~at_header_fields.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = ME.

    ME->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SET_URL.

    DATA: lva_val       TYPE c,
          lva_url       TYPE string,
          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = me->zif_integracao_outbound~at_auth_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_url                = me->zif_integracao_outbound~at_auth_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = me->zif_integracao_inject~co_request_method_post.

    CASE me->at_token-tipo.
      WHEN '1'.
      WHEN '2'.
        me->zif_integracao_inject~at_info_request_http-ds_content_type   = 'application/x-www-form-urlencoded'.
    ENDCASE.


  ENDMETHOD.
ENDCLASS.
