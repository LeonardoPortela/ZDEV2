class ZCL_INT_OB_CLIENTE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_OB_CLIENTE .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE value '171' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_INT_OB_CLIENTE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE    = me->at_id_interface.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO   = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL        = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_SINCRONIA    = ZIF_INTEGRACAO=>AT_TP_SINCRONIA_SINCRONA.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_NAO.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_SIM.

    ME->ZIF_INT_OB_CLIENTE~SET_DS_URL( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_int_ob_token_ecommerce(
               zcl_int_ob_token_ecommerce=>zif_int_ob_token_ecommerce~get_instance(
                 )->set_usuario_senha( "EXPORTING I_SENHA = ME->ZIF_INTEGRACAO_COF_KUHLMANN~AT_SENHA
                                       "          I_USUARIO = ME->ZIF_INTEGRACAO_COF_KUHLMANN~AT_USUARIO
                 )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  METHOD zif_int_ob_cliente~get_id_referencia.
    r_if_int_ob_cliente = me.
    e_referencia-tp_referencia = 'CLIENTE_COFIRMACAO'.
    e_referencia-id_referencia = me->zif_int_ob_cliente~at_id_cliente.
  ENDMETHOD.


  METHOD zif_int_ob_cliente~get_instance.
    IF zif_int_ob_cliente~at_if_int_ob_cliente IS NOT BOUND.
      CREATE OBJECT zif_int_ob_cliente~at_if_int_ob_cliente
        TYPE zcl_int_ob_cliente.
    ENDIF.
    r_if_int_ob_cliente = zif_int_ob_cliente~at_if_int_ob_cliente.
  ENDMETHOD.


  METHOD zif_int_ob_cliente~get_json.
    r_if_int_ob_cliente = me.

    e_json = '{' && cl_abap_char_utilities=>newline &&
             ' "sap_id" : ' && '"' && me->zif_int_ob_cliente~at_id_cliente && '"' && ' ' && cl_abap_char_utilities=>newline &&
             '}'.

    CONDENSE e_json NO-GAPS.
  ENDMETHOD.


  METHOD zif_int_ob_cliente~set_cliente_confirmar.
    r_if_int_ob_cliente = me.

    me->zif_int_ob_cliente~at_id_cliente = i_id_cliente.
    me->zif_int_ob_cliente~at_id_origem = i_id_origem.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_int_ob_cliente~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

  ENDMETHOD.


  METHOD zif_int_ob_cliente~set_ds_data.
    "Incluir Texto JSON para integração
    r_if_int_ob_cliente = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  METHOD zif_int_ob_cliente~set_ds_url.
    r_if_int_ob_cliente = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zauth_webservice
     WHERE service EQ 'AMAGGION_ENVIA_ID_CLIENTE_SAP'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'AMAGGION_ENVIA_ID_CLIENTE_SAP' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'AMAGGION_ENVIA_ID_CLIENTE_SAP'.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && me->zif_int_ob_cliente~at_id_origem. "me->zif_int_ob_cliente~at_id_cliente.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_int_ob_cliente~set_id_referencia( ).
  ENDMETHOD.


  METHOD zif_int_ob_cliente~set_id_referencia.
    "Incluir Chave de Referência
    r_if_int_ob_cliente = me.
    me->zif_int_ob_cliente~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_int_ob_cliente~set_send_msg.
    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_int_ob_cliente = me.

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

    CLEAR: lc_integrar.
  ENDMETHOD.
ENDCLASS.
