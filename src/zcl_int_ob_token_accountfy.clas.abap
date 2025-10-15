class ZCL_INT_OB_TOKEN_ACCOUNTFY definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_OB_TOKEN_ACCOUNTFY .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
protected section.
private section.

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE value '174' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_INT_OB_TOKEN_ACCOUNTFY IMPLEMENTATION.


  METHOD constructor.

    me->zif_int_ob_token_accountfy~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
*    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_int_ob_token_accountfy~set_ds_url( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  endmethod.


  METHOD zif_integracao_inject~set_form_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields  = i_form_fields.

    APPEND VALUE #( name = 'client_id'      value = 'amaggi_pt' ) TO me->zif_integracao_inject~at_form_fields.
    APPEND VALUE #( name = 'client_secret'  value = '319e2954-e2cd-42a3-b37f-160adac296e9' ) TO me->zif_integracao_inject~at_form_fields.
    APPEND VALUE #( name = 'grant_type'     value = '' ) TO me->zif_integracao_inject~at_form_fields.
    APPEND VALUE #( name = 'refresh_token'  value = 'refresh_token' ) TO me->zif_integracao_inject~at_form_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
*    APPEND VALUE #( name = '~request_method'      value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'Content-Type'         value = 'application/x-www-form-urlencoded' ) TO me->zif_integracao_inject~at_header_fields.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  METHOD zif_integracao_inject~set_parametro.
    r_if_integracao_inject = me.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INT_OB_TOKEN_ACCOUNTFY~GET_ID_REFERENCIA.
    r_if_int_ob_token_accountfy = me.
    e_referencia-tp_referencia = 'ACCOUNTFY_TOKEN'.
    e_referencia-id_referencia = me->zif_int_ob_token_accountfy~at_usuario.
  endmethod.


  METHOD zif_int_ob_token_accountfy~get_instance.

    IF zif_int_ob_token_accountfy~at_if_token_cliente IS NOT BOUND.
      CREATE OBJECT zif_int_ob_token_accountfy~at_if_token_cliente
        TYPE zcl_int_ob_token_accountfy
        EXPORTING
          i_servico = i_servico.
    ENDIF.

    IF i_servico IS NOT INITIAL.
      zif_int_ob_token_accountfy~at_servico = i_servico.
    ENDIF.

    r_if_int_ob_token_accountfy = zif_int_ob_token_accountfy~at_if_token_cliente.

  ENDMETHOD.


  METHOD zif_int_ob_token_accountfy~get_json.

    r_if_int_ob_token_accountfy = me.

    e_json = '{' && cl_abap_char_utilities=>newline &&
             ' "client_id" : "' && me->zif_int_ob_token_accountfy~at_usuario && '", ' && cl_abap_char_utilities=>newline &&
             ' "client_secret" : "' && me->zif_int_ob_token_accountfy~at_senha && '" ' && cl_abap_char_utilities=>newline &&
             ' "grant_type" : "" ' && cl_abap_char_utilities=>newline &&
             ' "refresh_token" : "refresh_token" ' && cl_abap_char_utilities=>newline &&

             '}'.

    e_json = ''.
    CONDENSE e_json NO-GAPS.

  ENDMETHOD.


  METHOD zif_int_ob_token_accountfy~set_ds_data.
    r_if_int_ob_token_accountfy = me.
*    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
    me->zif_integracao_inject~at_info_request_http-ds_form_data = i_json.

*    me->zif_integracao_inject~AT_FORM_FIELDS.

  ENDMETHOD.


  METHOD zif_int_ob_token_accountfy~set_ds_url.
    DATA: zcl_int TYPE REF TO zcl_integracao.
    CREATE OBJECT zcl_int.

    r_if_int_ob_token_accountfy = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zauth_webservice
     WHERE service    EQ 'ACCOUNTFY_TOKEN'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'ACCOUNTFY_TOKEN' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'ACCOUNTFY_TOKEN'.
    ENDIF.

*    me->zif_integracao_inject~at_info_request_http-ds_formato = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.

    me->zif_int_ob_token_accountfy~at_usuario = wa_webservice-username.
    me->zif_int_ob_token_accountfy~at_senha   = wa_webservice-password.
    me->zif_int_ob_token_accountfy~set_id_referencia( ).


*    IF wa_webservice-username IS INITIAL.
*      wa_webservice-username = 'amaggi_pt'.
*    ENDIF.
*
*    IF wa_webservice-password IS INITIAL.
*      wa_webservice-password = '319e2954-e2cd-42a3-b37f-160adac296e9'.
*    ENDIF.
*
*
    APPEND VALUE #( name = '~request_method'      value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'         value = 'application/x-www-form-urlencoded' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_int_ob_token_accountfy~set_id_referencia.
    "Incluir Chave de Referência
    r_if_int_ob_token_accountfy = me.
    me->zif_int_ob_token_accountfy~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_int_ob_token_accountfy~set_send_msg.

    TYPES BEGIN OF ty_retorno.
    TYPES: accestoken TYPE string.
    TYPES END OF ty_retorno.

    r_if_int_ob_token_accountfy = me.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    DATA: lc_integrar_inject TYPE REF TO zcl_integracao_inject.

    CREATE OBJECT lc_integrar.
*    CREATE OBJECT lc_integrar_inject.

    "AOENNING.
    APPEND VALUE #( name = 'client_id'      value = 'amaggi_pt' ) TO lc_integrar->zif_integracao~at_form_fields.
    APPEND VALUE #( name = 'client_secret'  value = '319e2954-e2cd-42a3-b37f-160adac296e9' ) TO lc_integrar->zif_integracao~at_form_fields.
*    lc_integrar_inject->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    "AOENNING.

**RJF
    APPEND VALUE #( name = 'grant_type'     value = '' ) TO lc_integrar->zif_integracao~at_form_fields..
    APPEND VALUE #( name = 'refresh_token'  value = 'refresh_token' ) TO lc_integrar->zif_integracao~at_form_fields..


    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    FREE: lc_integrar.
    CLEAR: lc_integrar.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

    e_access_token = lc_retorno-accestoken.
    CONDENSE e_access_token NO-GAPS.

    DATA(token_type)   = 'Bearer'.
    DATA(l_header_token) = |{ token_type } { e_access_token }|.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.


    APPEND VALUE #( name = 'Content-Type' value = 'multipart/form-data' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method' value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Accept' value = '*/*' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Accept-Encoding' value = 'gzip, deflate, br' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Accept-Encoding' value = 'multipart/form-data' ) TO me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'Content-Length' value = '' ) TO me->zif_integracao_inject~at_header_fields.

*    me->zif_integracao_inject~at_header_fields = VALUE #(
*                              ( name = 'Accept' value = '*/*' )
*                              ( name = 'Accept-Encoding' value = 'gzip, deflate, br' )
*                              ( name = 'Content-Type' value = 'multipart/form-data' )
*                              ).

  ENDMETHOD.


  METHOD zif_int_ob_token_accountfy~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_int_ob_token_accountfy~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_ob_token_accountfy~set_usuario_senha.

    TYPES: BEGIN OF ty_retorno,
             authenticationtoken TYPE string,
           END OF ty_retorno.

    data authenticationtoken TYPE string.

    DATA: lc_retorno  TYPE ty_retorno.

    r_if_int_ob_token_accountfy = me.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_int_ob_token_accountfy~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg(
           IMPORTING
             e_id_integracao = e_id_integracao
             e_access_token  = e_access_token
      ).


  ENDMETHOD.
ENDCLASS.
