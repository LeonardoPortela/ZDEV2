class ZCL_INT_OB_CREATE_INCIDENTE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '226' ##NO_TEXT.
  data AT_BODY type STRING .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_OB_CREATE_INCIDENTE IMPLEMENTATION.


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


  method zif_integracao_outbound~build_info_request.

    data: zvg_body type string.

    r_if_integracao_outbound = me.

    zvg_body = i_info_request.

    me->at_BODY = zvg_body.

  endmethod.


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


  method zif_integracao_outbound~get_data.

    r_if_integracao_outbound = me.

    clear: e_data.

*    call method /ui2/cl_json=>serialize
*      exporting
*        data   = me->at_BODY
*      receiving
*        r_json = e_data.

    e_data = me->at_BODY.


  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
*    e_referencia-tp_referencia = 'DUE_COMEX'.
*    e_referencia-id_referencia = me->at_dados_due-id_due.

  endmethod.


  method zif_integracao_outbound~get_instance.

    if zif_integracao_outbound~at_if_integracao_outbound is not bound.
      create object zif_integracao_outbound~at_if_integracao_outbound type zcl_int_OB_CREATE_INCIDENTE.
    endif.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  endmethod.


  method zif_integracao_outbound~send_msg.

    data: lc_integrar type ref to zcl_integracao.

    r_if_integracao_outbound = me.

    create object lc_integrar.


    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = cast #( me )
      )->set_new_msg( importing e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( importing e_integracao = e_integracao
      )->free(
      ).

    clear: lc_integrar.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = ME.

    ME->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  method zif_integracao_outbound~set_url.

    data: lva_url       type string.
    data: lva_url_token type string.

    r_if_integracao_outbound = me.

    select single *
      from zauth_webservice into @data(lwa_webservice)
     where service = 'OB_CREATE_INCIDENTE'.

    if sy-subrc is not initial.
      raise exception type zcx_integracao
        exporting
          textid = value #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'OB_CREATE_INCIDENTE' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'OB_CREATE_INCIDENTE'.
    endif.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    lva_url = lwa_webservice-url.
*    lva_url_token = lwa_webservice-url_token.


*02	Homologação
*03	Produção
    clear: me->zif_integracao_inject~at_header_fields.
    if sy-sysid eq 'PRD'.
      append value #( name = 'dry-run'   value = 'false' ) to me->zif_integracao_inject~at_header_fields.
    else.
      append value #( name = 'dry-run'   value = 'true' ) to me->zif_integracao_inject~at_header_fields.
    endif.

*    case sy-sysid.
*      when 'QAS'.
*        append value #( name = 'dry-run'   value = 'true' ) to me->zif_integracao_inject~at_header_fields.
*      when 'PRD'.
*        append value #( name = 'dry-run'   value = 'false' ) to me->zif_integracao_inject~at_header_fields.
*    endcase.

    append value #( name = 'Content-Type'   value = 'application/json' ) to me->zif_integracao_inject~at_header_fields.
    append value #( name = 'Authorization'   value = lwa_webservice-token ) to me->zif_integracao_inject~at_header_fields.


    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = 'application/json'.
    me->zif_integracao_inject~at_info_request_http-ds_url              = lva_url.
*    me->zif_integracao_inject~at_info_request_http-ds_url_token        = lva_url_token.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  endmethod.
ENDCLASS.
