class ZCL_INT_ORDEM_CARREGA_OPUS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_ORDEM_CARREGA_OPUS .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_ORDEM_CARREGA_OPUS IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_o_car_opus.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.

*    DATA: lva_id_referencia TYPE zintegracao-id_referencia,
*          lva_id_integracao TYPE zintegracao-id_integracao.
*
*    lva_id_integracao =  c_integracao-id_integracao.
*    lva_id_referencia =  me->zif_integracao_tcot_contratos~at_id_referencia.
*
*    UPDATE zintegracao
*         SET id_referencia = lva_id_referencia
*           WHERE id_integracao        = lva_id_integracao
*           AND   id_interface         = '040'.
*    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_int_ordem_carrega_opus~get_instance.

    IF zif_int_ordem_carrega_opus~at_int_ordem_carrega_opus IS NOT BOUND.
      CREATE OBJECT zif_int_ordem_carrega_opus~at_int_ordem_carrega_opus
        TYPE zcl_int_ordem_carrega_opus.
    ENDIF.
    r_if_int_ordem_carrega_opus = zif_int_ordem_carrega_opus~at_int_ordem_carrega_opus.
  ENDMETHOD.


  METHOD zif_int_ordem_carrega_opus~get_json.
    r_if_int_ordem_carrega_opus = me.

    me->zif_int_ordem_carrega_opus~monta_json( EXPORTING i_data = me->zif_int_ordem_carrega_opus~at_data RECEIVING e_json = DATA(lc_json) ).
    e_json = lc_json.

  ENDMETHOD.


  METHOD zif_int_ordem_carrega_opus~monta_json.

    CLEAR: e_json.

    CHECK me->zif_int_ordem_carrega_opus~at_data IS NOT INITIAL.

    IF me->zif_int_ordem_carrega_opus~at_data-id_ordem IS INITIAL.

* Construindo JSON.
      e_json = '{ ' && ' "safra": " ' && me->zif_int_ordem_carrega_opus~at_data-nr_safra  && '", '
                   && ' "numeroOrdem": " ' && me->zif_int_ordem_carrega_opus~at_data-nr_ordem  && '", '
                   && ' "status": " ' && me->zif_int_ordem_carrega_opus~at_data-tp_status && '" '
                   && ' }'.
    ELSE.
      E_JSON = '{ ' &&
               ' "idOrdem": ' && me->zif_int_ordem_carrega_opus~at_data-id_ordem && ',  ' &&
               ' "status": " ' && me->zif_int_ordem_carrega_opus~at_data-tp_status && '"  ' &&
               ' }'.

    ENDIF.



  ENDMETHOD.


  METHOD zif_int_ordem_carrega_opus~post_ordem_car_opus.

    r_if_int_ordem_carrega_opus = me.

        "Inclui Json na Mesagem a Ser Enviada
        me->zif_int_ordem_carrega_opus~get_json( IMPORTING e_json = DATA(lc_json)
          )->set_ds_data( EXPORTING i_json = lc_json
          )->set_ds_url(
          )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
          ).

        /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data ).

  ENDMETHOD.


  METHOD zif_int_ordem_carrega_opus~set_dados_ordem_carregamento.

    r_if_int_ordem_carrega_opus = me.

    FREE: me->zif_int_ordem_carrega_opus~at_data.
    me->zif_int_ordem_carrega_opus~at_data = i_data.
  ENDMETHOD.


  METHOD zif_int_ordem_carrega_opus~set_ds_data.
    "Incluir Texto JSON para integração
    r_if_int_ordem_carrega_opus = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  ENDMETHOD.


  METHOD zif_int_ordem_carrega_opus~set_ds_url.

    r_if_int_ordem_carrega_opus = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ 'O'
       AND servico EQ '10'. "@me->zif_integracao_comb~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UD'
                            attr2 = me->zif_int_ordem_carrega_opus~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_int_ordem_carrega_opus~at_servico ).
    ENDIF.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Token'            value = wa_webservice-senha ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token          = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   =  ''.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
  ENDMETHOD.


  method ZIF_INT_ORDEM_CARREGA_OPUS~SET_ID_REFERENCIA.
  endmethod.


  method ZIF_INT_ORDEM_CARREGA_OPUS~SET_INT_COMB.
  endmethod.


  METHOD zif_int_ordem_carrega_opus~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_int_ordem_carrega_opus = me.

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
