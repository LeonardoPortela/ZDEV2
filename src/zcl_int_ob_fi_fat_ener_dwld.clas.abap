*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZCL_INT_IB_FI_FAT_ENER_DWLD                                                          *
*& Chamado        : USER STORY 140931                                                                    *
*& Data           : 23/12/2024                                                                           *
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
class ZCL_INT_OB_FI_FAT_ENER_DWLD definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

* Busca Fatura Energia - Download Fatura
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '252' ##NO_TEXT.
  constants:
    lc_servico       TYPE c LENGTH 20 value 'FATURA_ENERGIA_DWLD' ##NO_TEXT.
  constants:
    lc_authorization TYPE c LENGTH 13 value 'Authorization' ##NO_TEXT.
  constants:
    lc_content_type  TYPE c LENGTH 04 value 'JSON' ##NO_TEXT.
  constants:
    lc_interface     TYPE c LENGTH 09 value 'interface' ##NO_TEXT.
  constants:
    lc_erro          TYPE c LENGTH 01 value 'E' ##NO_TEXT.
  data AT_PARAMS type ZFICT_FATURA_ENERGIA_INVOIFILE .
  data AT_BODY type ZFICT_FATURA_ENERGIA_INVOIFILE .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_INT_OB_FI_FAT_ENER_DWLD IMPLEMENTATION.


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

    e_data = e_data+1.
    DATA(vl_len) = strlen( e_data ) - 1.
    e_data = e_data(vl_len).

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.
    r_if_integracao_outbound = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_fi_fat_ener_dwld.
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


  METHOD ZIF_INTEGRACAO_OUTBOUND~SET_URL.

    CONSTANTS: cl_apikeyclient TYPE string VALUE 'ApiKeyClient',
               cl_apikeyauth   TYPE string VALUE 'ApiKeyAuth',
               cl_content_type TYPE string VALUE 'Content-Type'.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice
      INTO @DATA(lwa_webservice)
     WHERE service EQ @lc_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = CONV #( TEXT-001 ) "// Serviço não configurado:
                            attr2 = CONV #( me->lc_servico ) )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = me->lc_erro "// E
          msgv1  = CONV #( TEXT-001 ) "// Serviço não configurado:
          msgv2  = CONV #( me->lc_servico ).
    ENDIF.

    DATA(v_url) = |{ lwa_webservice-url }{ lwa_webservice-add01 }|.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            = me->lc_content_type.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = v_url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = lwa_webservice-method.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_false.

*** Informação do header
    APPEND VALUE #( name = cl_apikeyclient value = lwa_webservice-username )     TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = cl_apikeyauth   value = lwa_webservice-password )     TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = cl_content_type value = lwa_webservice-content_type ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
