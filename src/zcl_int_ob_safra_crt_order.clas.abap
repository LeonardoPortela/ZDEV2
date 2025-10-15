class ZCL_INT_OB_SAFRA_CRT_ORDER definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '289' ##NO_TEXT.
  constants:
    lc_servico TYPE c LENGTH 19 value 'ORDEM_SAFRA_CONTROL' ##NO_TEXT.
  constants:
    lc_authorization TYPE c LENGTH 13 value 'Authorization' ##NO_TEXT.
  constants:
    lc_basic         TYPE c LENGTH 06 value 'Bearer' ##NO_TEXT.
  constants:
    lc_erro          TYPE c LENGTH 01 value 'E' ##NO_TEXT.
  constants:
    lc_interface TYPE c LENGTH 09 value 'interface' ##NO_TEXT.
  data AT_BODY type ZDE_SAFRA_CONTROL_ORDEM .
  data AT_EXTERNALID type ZDE_SAFRA_OD_EXTERNALID .
  constants:
    lc_content_type  TYPE c LENGTH 04 value 'JSON' ##NO_TEXT.
  data AT_ID_REFERENCIA type STRING .

  methods CONSTRUCTOR .
  methods SET_METODO_HTTP
    importing
      !I_METODO type STRING .
  methods GET_METODO_HTTP
    returning
      value(E_METODO) type STRING .
protected section.
private section.

  data AT_METODO_HTTP type STRING.
ENDCLASS.



CLASS ZCL_INT_OB_SAFRA_CRT_ORDER IMPLEMENTATION.


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


  method GET_METODO_HTTP.
    e_metodo = at_metodo_http.
  endmethod.


  method SET_METODO_HTTP.
     at_metodo_http = i_metodo.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


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


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


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
    IF at_metodo_http EQ zif_integracao_inject=>co_request_method_post or
       at_metodo_http EQ zif_integracao_inject=>co_request_method_put .
      MOVE-CORRESPONDING i_info_request TO at_body.
      MOVE-CORRESPONDING i_info_request TO at_externalid.
    ELSE.
      MOVE-CORRESPONDING i_info_request TO at_externalid.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

    DATA: ls_order       TYPE zde_safra_control_ordem.

    r_if_integracao_outbound = me.

    ls_order             = i_info_request.
    me->at_id_referencia = ls_order-externalid.

    DATA(lv_metodo) = me->get_metodo_http( ).

    IF lv_metodo IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_metodo = lv_metodo.
    ENDIF.

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

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = me->at_body
      RECEIVING
        r_json = e_data.

    TRANSLATE e_data TO LOWER CASE.

    REPLACE ALL OCCURRENCES OF 'c-'                          IN e_data WITH 'C-'.
    REPLACE ALL OCCURRENCES OF 'f-'                          IN e_data WITH 'F-'.
    REPLACE ALL OCCURRENCES OF 'externalid'                  IN e_data WITH 'externalId'.
    REPLACE ALL OCCURRENCES OF 'externalorderid'             IN e_data WITH 'externalOrderId'.
    REPLACE ALL OCCURRENCES OF 'freighttype'                 IN e_data WITH 'freightType'.
    REPLACE ALL OCCURRENCES OF 'duedate'                     IN e_data WITH 'dueDate'.
    REPLACE ALL OCCURRENCES OF 'branchoffice'                IN e_data WITH 'branchOffice'.
    REPLACE ALL OCCURRENCES OF 'withdrawalplace'             IN e_data WITH 'withdrawalPlace'.
    REPLACE ALL OCCURRENCES OF 'specialattention'            IN e_data WITH 'specialAttention'.
    REPLACE ALL OCCURRENCES OF 'specialattentiondescription' IN e_data WITH 'specialAttentionDescription'.
    REPLACE ALL OCCURRENCES OF 'customfields'                IN e_data WITH 'customFields'.
    REPLACE ALL OCCURRENCES OF '"true"'                      IN e_data WITH 'true'.
    REPLACE ALL OCCURRENCES OF '"false"'                     IN e_data WITH 'false'.
    REPLACE ALL OCCURRENCES OF '"null"'                      IN e_data WITH 'null'.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound   = me.
    e_referencia-tp_referencia = 'OB_SAFRA_ORDER'.
    e_referencia-id_referencia = me->at_id_referencia.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_safra_crt_order.
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
      )->set_outbound_msg( IMPORTING e_integracao = e_integracao
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

    DATA: zcl_base64encoder TYPE REF TO cl_http_utility.
    CREATE OBJECT zcl_base64encoder.

    r_if_integracao_outbound = me.

    DATA(lv_metodo) = at_metodo_http.
    DATA(lv_externalid) = at_externalid-externalid.

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

    CASE lv_metodo.
      WHEN zif_integracao_inject=>co_request_method_post.
        DATA(v_url) = |{ lwa_webservice-url }|.

      WHEN zif_integracao_inject=>co_request_method_get.
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.

      WHEN zif_integracao_inject=>co_request_method_put.
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.

      WHEN zif_integracao_inject=>co_request_method_delete.
        v_url = |{ lwa_webservice-url }/{ lv_externalid }|.
    ENDCASE.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            = me->lc_content_type.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = v_url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = lv_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_false.

    DATA(lv_tpcredentials) = lwa_webservice-token.

    APPEND VALUE #( name = lc_authorization value = |{ lc_basic } { lv_tpcredentials }| ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
