class ZCL_INT_OB_ZSDT0051_LEGADOS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '139' ##NO_TEXT.
  data AT_DADOS type ZSDT_ZSDT0257 .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_OB_ZSDT0051_LEGADOS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_sim.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'interface'.

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

    r_if_integracao_outbound = me.

    MOVE-CORRESPONDING i_info_request TO me->at_dados.

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


  method ZIF_INTEGRACAO_OUTBOUND~GET_DATA.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

    CALL METHOD /ui2/cl_json=>serialize
       EXPORTING
         data   = me->at_dados
       RECEIVING
         r_json = e_data.


  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
*    e_referencia-tp_referencia = 'DUE_COMEX'.
*    e_referencia-id_referencia = me->at_dados_due-id_due.

  endmethod.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_zsdt0051_legados.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

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

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = ME.

    ME->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  METHOD zif_integracao_outbound~set_url.

    DATA: lva_url       TYPE string.
    DATA: lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'DADOS_ADIANTAMENTO'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'DADOS_ADIANTAMENTO' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'DADOS_ADIANTAMENTO'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    lva_url       = lwa_webservice-url.
    lva_url_token = lwa_webservice-url_token.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url              = lva_url.
    me->zif_integracao_inject~at_info_request_http-ds_url_token        = lva_url_token.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.
ENDCLASS.
