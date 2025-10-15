*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZCL_INT_UB_FI_FAT_ENER_UPST                                                          *
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
CLASS zcl_int_ob_fi_fat_ener_upst DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_outbound .

* Busca Fatura Energia - Atualiza status Fatura
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '253' ##NO_TEXT.
    CONSTANTS:
      lc_servico       TYPE c LENGTH 20 VALUE 'FATURA_ENERGIA_UPST' ##NO_TEXT.
    CONSTANTS:
      lc_authorization TYPE c LENGTH 13 VALUE 'Authorization' ##NO_TEXT.
    CONSTANTS:
      lc_content_type  TYPE c LENGTH 04 VALUE 'JSON' ##NO_TEXT.
    CONSTANTS:
      lc_interface     TYPE c LENGTH 09 VALUE 'interface' ##NO_TEXT.
    CONSTANTS:
      lc_erro          TYPE c LENGTH 01 VALUE 'E' ##NO_TEXT.
    DATA at_params TYPE zfie_fatura_energia_upst .
    DATA at_body TYPE zfie_fatura_energia_upst .

    METHODS constructor
      IMPORTING
        VALUE(i_servico) TYPE ztipowebserv OPTIONAL
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_INT_OB_FI_FAT_ENER_UPST IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = me->lc_interface.

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
  ENDMETHOD.


  METHOD zif_integracao_inject~set_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


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


  METHOD zif_integracao_outbound~build_info_request.
    r_if_integracao_outbound = me.
    MOVE-CORRESPONDING i_info_request TO at_params.
  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

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


  METHOD zif_integracao_outbound~get_data.

    CLEAR: e_data.

    r_if_integracao_outbound = me.

    MOVE-CORRESPONDING at_params TO me->at_body.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = me->at_body
      RECEIVING
        r_json = e_data.

    REPLACE ALL OCCURRENCES OF '"null"' IN e_data WITH 'null'.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.
    r_if_integracao_outbound = me.
  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_fi_fat_ener_upst.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  METHOD zif_integracao_outbound~send_msg.

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


  METHOD zif_integracao_outbound~set_data.

    r_if_integracao_outbound = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_id_referencia.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_url.

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
