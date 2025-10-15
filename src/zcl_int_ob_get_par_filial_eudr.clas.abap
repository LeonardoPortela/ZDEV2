CLASS zcl_int_ob_get_par_filial_eudr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_outbound .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '231' ##NO_TEXT.
    CONSTANTS:
      lc_servico       TYPE c LENGTH 25 VALUE 'EUDR_GET_PARAMETRO_FILIAL' ##NO_TEXT.
    CONSTANTS:
      lc_authorization TYPE c LENGTH 13 VALUE 'Authorization' ##NO_TEXT.
    CONSTANTS:
      lc_content_type  TYPE c LENGTH 04 VALUE 'JSON' ##NO_TEXT.
    CONSTANTS:
      lc_interface     TYPE c LENGTH 09 VALUE 'interface' ##NO_TEXT.
    CONSTANTS:
      lc_erro          TYPE c LENGTH 01 VALUE 'E' ##NO_TEXT.
    DATA at_dados_ordem_orc TYPE zpm_ordem_orc .
    DATA at_params TYPE zstruct_get_filial_eudr .
    DATA lv_bukrs TYPE bukrs.
    DATA at_body TYPE znfwe0001 . "// Estrutura do JSON caso exista para inserir no BODY

    METHODS constructor
      IMPORTING
        VALUE(i_servico) TYPE ztipowebserv OPTIONAL
      RAISING
        zcx_integracao .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_INT_OB_GET_PAR_FILIAL_EUDR IMPLEMENTATION.


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

    CHECK at_params-idfilialsap IS NOT INITIAL.

    CLEAR lv_bukrs.

    "// Recupera a Empresa e a Filial validando sua existencia
    SELECT SINGLE bukrs
      FROM j_1bbranch
      INTO ( lv_bukrs )
      WHERE branch EQ at_params-idfilialsap.

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

*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data   = me->at_body
*      RECEIVING
*        r_json = e_data.
*
    TRANSLATE e_data TO LOWER CASE.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.
    r_if_integracao_outbound = me.
  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_get_par_filial_eudr.
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

    REPLACE '[IDEMPRESASAP]' IN v_url WITH lv_bukrs.
    REPLACE '[IDFILIALSAP]'  IN v_url WITH at_params-idfilialsap.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http =
    VALUE #(
                ds_formato            = me->lc_content_type
                ds_content_type       = lwa_webservice-content_type
                ds_url                = v_url
"//             ds_url_token          = lwa_webservice-token
"//             ds_server_protocolo   = abap_off
                ds_metodo             = lwa_webservice-method
                ds_not_content_length = abap_false
    ).

    "// Informação do header
    APPEND VALUE #( name = me->lc_authorization value = lwa_webservice-token ) TO me->zif_integracao_inject~at_header_fields.
    "// APPEND VALUE #( name = 'Content-Type'  value = 'application/x-www-form-urlencoded' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
