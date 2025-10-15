class ZCL_INT_OB_TOKEN_ECOMMERCE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_OB_TOKEN_ECOMMERCE .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
protected section.
private section.

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE value '169' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_INT_OB_TOKEN_ECOMMERCE IMPLEMENTATION.


  METHOD constructor.

    me->zif_int_ob_token_ecommerce~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_int_ob_token_ecommerce~set_ds_url( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
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


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  METHOD zif_int_ob_token_ecommerce~get_id_referencia.
    r_if_int_ob_token_ecommerce = me.
    e_referencia-tp_referencia = 'TOKEN_ECOMMERCE'.
    e_referencia-id_referencia = me->zif_int_ob_token_ecommerce~at_usuario.
  ENDMETHOD.


  METHOD zif_int_ob_token_ecommerce~get_instance.
    IF zif_int_ob_token_ecommerce~at_if_token_cliente IS NOT BOUND.
      CREATE OBJECT zif_int_ob_token_ecommerce~at_if_token_cliente
        TYPE zcl_int_ob_token_ecommerce
        EXPORTING
          i_servico = i_servico.
    ENDIF.

    IF i_servico IS NOT INITIAL.
      zif_int_ob_token_ecommerce~at_servico = i_servico.
    ENDIF.

    r_if_int_ob_token_ecommerce = zif_int_ob_token_ecommerce~at_if_token_cliente.
  ENDMETHOD.


  METHOD ZIF_INT_OB_TOKEN_ECOMMERCE~GET_JSON.
    r_if_int_ob_token_ecommerce = me.

    e_json = '{' && cl_abap_char_utilities=>newline &&
             ' "username" : "' && me->zif_int_ob_token_ecommerce~at_usuario && '", ' && cl_abap_char_utilities=>newline &&
             ' "password" : "' && me->zif_int_ob_token_ecommerce~at_senha && '" ' && cl_abap_char_utilities=>newline &&
             '}'.

    CONDENSE e_json NO-GAPS.

  ENDMETHOD.


  METHOD ZIF_INT_OB_TOKEN_ECOMMERCE~SET_DS_DATA.
    r_if_int_ob_token_ecommerce = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  ENDMETHOD.


  METHOD zif_int_ob_token_ecommerce~set_ds_url.
    r_if_int_ob_token_ecommerce = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zauth_webservice
     WHERE service    EQ 'AMAGGION_TOKEN'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'AMAGGION_TOKEN' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'AMAGGION_TOKEN'.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.

    me->zif_int_ob_token_ecommerce~at_usuario = wa_webservice-username.
    me->zif_int_ob_token_ecommerce~at_senha   = wa_webservice-password.
    me->zif_int_ob_token_ecommerce~set_id_referencia( ).
  ENDMETHOD.


  METHOD ZIF_INT_OB_TOKEN_ECOMMERCE~SET_ID_REFERENCIA.
    "Incluir Chave de Referência
    r_if_int_ob_token_ecommerce = me.
    me->zif_int_ob_token_ecommerce~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD ZIF_INT_OB_TOKEN_ECOMMERCE~SET_SEND_MSG.
    TYPES BEGIN OF ty_retorno.
    TYPES: token TYPE string.
    TYPES END OF ty_retorno.

    r_if_int_ob_token_ecommerce = me.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    CREATE OBJECT lc_integrar.

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

    "/ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

    e_access_token = e_integracao-ds_data_retorno.

    REPLACE ALL OCCURRENCES OF '"' IN e_access_token WITH ''.
    CONDENSE e_access_token NO-GAPS.

  ENDMETHOD.


  METHOD ZIF_INT_OB_TOKEN_ECOMMERCE~SET_SERVICO.
    IF i_servico IS NOT INITIAL.
      zif_int_ob_token_ecommerce~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_ob_token_ecommerce~set_usuario_senha.
    r_if_int_ob_token_ecommerce = me.

    "ME->ZIF_INTEGRACAO_TOKEN_cliente~AT_USUARIO = I_USUARIO.
    "ME->ZIF_INTEGRACAO_TOKEN_cliente~AT_SENHA = I_SENHA.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_int_ob_token_ecommerce~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg(
           IMPORTING
             e_id_integracao = e_id_integracao
             e_access_token  = e_access_token
      ).

    REPLACE ALL OCCURRENCES OF '"' IN e_access_token WITH ''.
    CONDENSE e_access_token NO-GAPS.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'  value = |Bearer { e_access_token }| ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
