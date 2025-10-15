class ZCL_INT_TOKEN_TRACE_ACTS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_TOKEN_TRACE_ACTS .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_TOKEN_TRACE_ACTS IMPLEMENTATION.


  METHOD constructor.

    me->zif_int_token_trace_acts~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_acts_fardinho.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_int_token_trace_acts~set_ds_url( ).

  ENDMETHOD.


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


  METHOD zif_int_token_trace_acts~get_id_referencia.
    r_if_int_token_trace_acts = me.
    e_referencia-tp_referencia = 'TRACE_ACTS_TOKEN'.
    e_referencia-id_referencia = me->zif_int_token_trace_acts~at_usuario.
  ENDMETHOD.


  METHOD zif_int_token_trace_acts~get_instance.
    IF zif_int_token_trace_acts~at_if_token_trace_acts IS NOT BOUND.
      CREATE OBJECT zif_int_token_trace_acts~at_if_token_trace_acts
        TYPE zcl_int_token_trace_acts
        EXPORTING
          i_servico = i_servico.   " Tipo de Serviço WebService
*        CATCH zcx_integracao.    " .
    ENDIF.

    IF i_servico IS NOT INITIAL.
      zif_int_token_trace_acts~at_servico = i_servico.
    ENDIF.

    r_if_int_token_trace_acts = zif_int_token_trace_acts~at_if_token_trace_acts.
  ENDMETHOD.


  METHOD zif_int_token_trace_acts~get_json.

    r_if_int_token_trace_acts = me.

    e_json = '{' && cl_abap_char_utilities=>newline &&
             ' "username" : "' && me->zif_int_token_trace_acts~at_usuario && '", ' && cl_abap_char_utilities=>newline &&
             ' "password" : "' && me->zif_int_token_trace_acts~at_senha && '" ' && cl_abap_char_utilities=>newline &&
             '}'.

  ENDMETHOD.


  method ZIF_INT_TOKEN_TRACE_ACTS~SET_DS_DATA.

    R_IF_int_token_trace_acts = ME.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.
  endmethod.


  method ZIF_INT_TOKEN_TRACE_ACTS~SET_DS_URL.

    r_if_int_token_trace_acts = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo EQ '6'
       AND servico EQ '23'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_int_token_trace_acts~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = '6'
          msgv2  = CONV #( me->zif_int_token_trace_acts~at_servico ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url_token.
*    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = ''."zif_integracao_token_kuhlmann=>at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_int_token_trace_acts~at_usuario = wa_webservice-usuario.
    me->zif_int_token_trace_acts~at_senha   = wa_webservice-senha.
    me->zif_int_token_trace_acts~set_id_referencia( ).
  endmethod.


  METHOD zif_int_token_trace_acts~set_id_referencia.


    "Incluir Chave de Referência
    r_if_int_token_trace_acts = me.
    me->zif_int_token_trace_acts~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_int_token_trace_acts~set_send_msg.


    TYPES BEGIN OF ty_retorno.
    TYPES: access_token TYPE string,
           login        TYPE string.
    TYPES END OF ty_retorno.

    r_if_int_token_trace_acts = me.

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

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

*    E_ACCESS_TOKEN =  E_INTEGRACAO-DS_DATA_RETORNO.
    e_access_token = lc_retorno-access_token
    .
  ENDMETHOD.


  METHOD zif_int_token_trace_acts~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_int_token_trace_acts~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  method ZIF_INT_TOKEN_TRACE_ACTS~SET_USUARIO_SENHA.

    R_IF_int_token_trace_acts = ME.

    "ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~AT_USUARIO = I_USUARIO.
    "ME->ZIF_INTEGRACAO_TOKEN_KUHLMANN~AT_SENHA = I_SENHA.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_int_token_trace_acts~GET_JSON( IMPORTING E_JSON = DATA(LC_JSON)
      )->SET_DS_DATA( EXPORTING I_JSON = LC_JSON
      )->SET_DS_URL(
      )->SET_SEND_MSG(
           IMPORTING
             E_ID_INTEGRACAO = E_ID_INTEGRACAO
             E_ACCESS_TOKEN  = E_ACCESS_TOKEN
      ).

    CLEAR: ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.
    APPEND VALUE #( NAME = 'Authorization'  VALUE = |Bearer { E_ACCESS_TOKEN }| ) TO ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.
ENDCLASS.
