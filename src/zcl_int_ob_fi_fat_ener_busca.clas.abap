*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZCL_INT_IB_FI_FAT_ENER_BUSCA                                                         *
*& Chamado        : USER STORY 140931                                                                    *
*& Data           : 20/12/2024                                                                           *
*& Especificado   : Antonio Rodrigues                                                                    *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 24/12/2024  |DEVK9A2C12  |NSEGATIN       |Desenvilvimento inicial. Chamado: 140931.                   *
*--------------------------------------------------------------------------------------------------------*
class ZCL_INT_OB_FI_FAT_ENER_BUSCA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

* Busca Fatura Energia - Faturamento/Pagamento
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '251' ##NO_TEXT.
  constants:
    lc_servico       TYPE c LENGTH 20 value 'FATURA_ENERGIA_BUSCA' ##NO_TEXT.
  constants:
    lc_authorization TYPE c LENGTH 13 value 'Authorization' ##NO_TEXT.
  constants:
    lc_content_type  TYPE c LENGTH 04 value 'JSON' ##NO_TEXT.
  constants:
    lc_interface     TYPE c LENGTH 09 value 'interface' ##NO_TEXT.
  constants:
    lc_erro          TYPE c LENGTH 01 value 'E' ##NO_TEXT.
  data AT_PARAMS type ZFIE_FATURA_ENERGIA_BUSCA .
  data AT_BODY type ZFIE_FATURA_ENERGIA_BUSCA .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_INT_OB_FI_FAT_ENER_BUSCA IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = me->lc_interface.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST.
    r_if_integracao_outbound = me.
    MOVE-CORRESPONDING i_info_request TO at_params.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~EXECUTE_REQUEST.

    r_if_integracao_outbound = me.

    "// Inclui Json na Mensagem a Ser Enviada
    me->zif_integracao_outbound~build_info_request( i_info_request = i_info_request
      )->get_data( IMPORTING e_data = DATA(lc_data)
      )->set_data( EXPORTING i_data = lc_data
      )->set_url(
      )->set_id_referencia(
      )->send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_DATA.

    CLEAR: e_data.

    r_if_integracao_outbound = me.

    MOVE-CORRESPONDING at_params TO me->at_body.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = me->at_body
      RECEIVING
        r_json = e_data.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.
    r_if_integracao_outbound = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_fi_fat_ener_busca.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_outbound = me.

    CREATE OBJECT lc_integrar.

    "// Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  method zif_integracao_outbound~set_url.

    constants: cl_apikeyclient type string value 'ApiKeyClient',
               cl_apikeyauth   type string value 'ApiKeyAuth',
               cl_content_type type string value 'Content-Type'.

    r_if_integracao_outbound = me.

    select single *
      from zauth_webservice
      into @data(lwa_webservice)
     where service eq @lc_servico.

    if sy-subrc is not initial.
      raise exception type zcx_integracao
        exporting
          textid = value #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = conv #( text-001 ) "// Serviço não configurado:
                            attr2 = conv #( me->lc_servico ) )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = me->lc_erro "// E
          msgv1  = conv #( text-001 ) "// Serviço não configurado:
          msgv2  = conv #( me->lc_servico ).
    endif.

    data(v_url) = |{ lwa_webservice-url }{ lwa_webservice-add01 }|.

    clear: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = v_url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = lwa_webservice-method.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

*** Informação do header
    append value #( name = cl_apikeyclient value = lwa_webservice-username )     to me->zif_integracao_inject~at_header_fields.
    append value #( name = cl_apikeyauth   value = lwa_webservice-password )     to me->zif_integracao_inject~at_header_fields.
*    append value #( name = cl_content_type value = lwa_webservice-content_type ) to me->zif_integracao_inject~at_header_fields.

  endmethod.
ENDCLASS.
