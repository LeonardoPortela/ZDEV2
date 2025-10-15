class ZCL_INT_REALIZADO_FRETE_SIGAM definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_REAL_FRE_SIGAM .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_REALIZADO_FRETE_SIGAM IMPLEMENTATION.


  method CONSTRUCTOR.
    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_real_fre_sigam.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'goflux'.
  endmethod.


  METHOD zif_integracao_inject~get_header_request_http.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  endmethod.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
        R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_real_fre_sigam~get_id_referencia.
    r_if_int_real_fre_sigam = me.
    e_referencia-tp_referencia = 'REALIZADO FRETE - SIGAM'.
    e_referencia-id_referencia = me->zif_integracao_real_fre_sigam~at_id_referencia.
  ENDMETHOD.


  METHOD zif_integracao_real_fre_sigam~get_instance.
    IF zif_integracao_real_fre_sigam~at_if_int_real_fre_sigam IS NOT BOUND.
      CREATE OBJECT zif_integracao_real_fre_sigam~at_if_int_real_fre_sigam
        TYPE zcl_int_realizado_frete_sigam.
    ENDIF.
    r_if_int_real_fre_sigam = zif_integracao_real_fre_sigam~at_if_int_real_fre_sigam.
  ENDMETHOD.


  METHOD zif_integracao_real_fre_sigam~get_json.
    r_if_int_real_fre_sigam = me.

    CHECK me->zif_integracao_real_fre_sigam~at_json IS NOT INITIAL.
    e_json = me->zif_integracao_real_fre_sigam~at_json.
  ENDMETHOD.


  METHOD zif_integracao_real_fre_sigam~get_metodo.
    r_if_int_real_fre_sigam = me.

    CHECK me->zif_integracao_real_fre_sigam~at_metodo IS NOT INITIAL.
    e_metodo = me->zif_integracao_real_fre_sigam~at_metodo.
  ENDMETHOD.


  method ZIF_INTEGRACAO_REAL_FRE_SIGAM~SET_DS_DATA.
    "Incluir Texto JSON para integração
    r_if_int_real_fre_sigam = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  endmethod.


  METHOD zif_integracao_real_fre_sigam~set_ds_url.
    r_if_int_real_fre_sigam = me.

    DATA: v_url      TYPE string.

    CHECK me->zif_integracao_real_fre_sigam~at_metodo IS NOT INITIAL.
    DATA(lva_metodo) = me->zif_integracao_real_fre_sigam~at_metodo.

    SELECT SINGLE * FROM zauth_webservice INTO @DATA(wa_webservice)
       WHERE service = 'REAL_FRETE_SIGAM'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'T'
                            attr2 = 'TC' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'T'
          msgv2  = 'TC'.
    ENDIF.

*    CASE lva_metodo.
*      WHEN 'POST'.
*        CONCATENATE wa_webservice-url 'cadastrar'    INTO v_url.
*      WHEN 'DELETE'.
*        CONCATENATE wa_webservice-url 'excluir'    INTO v_url.
*    ENDCASE.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo =  lva_metodo.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_integracao_real_fre_sigam~set_id_referencia( ).
  ENDMETHOD.


  METHOD zif_integracao_real_fre_sigam~set_id_referencia.
    "Incluir Chave de Referência
    r_if_int_real_fre_sigam = me.
    me->zif_integracao_real_fre_sigam~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_integracao_real_fre_sigam~set_int_real_frete_sigam.
    r_if_int_real_fre_sigam = me.

    me->zif_integracao_real_fre_sigam~at_json = i_json.
    me->zif_integracao_real_fre_sigam~at_metodo = i_metodo.
    me->zif_integracao_real_fre_sigam~at_id_referencia = i_id_referencia.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_real_fre_sigam~set_ds_url(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->get_metodo( IMPORTING e_metodo = DATA(lc_metodo)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).
  ENDMETHOD.


  METHOD zif_integracao_real_fre_sigam~set_send_msg.

    TYPES BEGIN OF ty_retorno.
    TYPES: descricao TYPE string.
    TYPES: status TYPE string.
    TYPES END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.


    r_if_int_real_fre_sigam = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    FREE: lc_integrar.
    CLEAR: lc_integrar.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

  "  e_status = zcl_string=>lpad( i_str  = lc_retorno-status i_qtd  = 2 i_char = '0' ).

  ENDMETHOD.
ENDCLASS.
