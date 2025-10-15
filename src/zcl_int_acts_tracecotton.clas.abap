class ZCL_INT_ACTS_TRACECOTTON definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_ACTS_TRACECOTTON .

  methods CONSTRUCTOR
    importing
      !I_SERVICO type ZTIPOWEBSERV
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_ACTS_TRACECOTTON IMPLEMENTATION.


  METHOD constructor.

    me->zif_int_acts_tracecotton~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_acts_fardinho.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

*    me->zif_int_acts_tracecotton~set_ds_url( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_int_token_trace_acts(
               zcl_int_token_trace_acts=>zif_int_token_trace_acts~get_instance(
                   i_servico = CONV #( me->zif_int_acts_tracecotton~at_servico )
                 )->set_ds_url(
                 )->set_usuario_senha( "EXPORTING I_SENHA = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA I_USUARIO = ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO
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


  METHOD zif_int_acts_tracecotton~get_id_referencia.

    r_if_int_acts_tracecotton = me.

    e_referencia-tp_referencia = 'TRACECOTTON_ACTS'.
    e_referencia-id_referencia = me->zif_int_acts_tracecotton~at_usuario.
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~get_instance.


    IF zif_int_acts_tracecotton~at_if_int_acts_tracecotton IS NOT BOUND.
      CREATE OBJECT zif_int_acts_tracecotton~at_if_int_acts_tracecotton
        TYPE zcl_int_acts_tracecotton
        EXPORTING
          i_servico = i_servico. " Tipo de Serviço WebService
*        CATCH zcx_integracao.    " .
    ENDIF.

    r_if_int_acts_tracecotton = zif_int_acts_tracecotton~at_if_int_acts_tracecotton.
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~get_json.
    r_if_int_acts_tracecotton = me.
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_cd_sai.
    r_if_int_acts_tracecotton = me.
    IF i_cd_sai IS NOT INITIAL.
      me->zif_int_acts_tracecotton~at_cd_sai = i_cd_sai.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_dados_acts.

    r_if_int_acts_tracecotton = me.

    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA = I_SENHA.
    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO = I_USUARIO.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_int_acts_tracecotton~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_return ).
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_ds_data.
    r_if_int_acts_tracecotton = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_ds_url.
    DATA: i_param TYPE string.
    r_if_int_acts_tracecotton = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '6'
       AND servico EQ @me->zif_int_acts_tracecotton~at_servico."'23'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_int_acts_tracecotton~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_int_acts_tracecotton~at_servico ).
    ENDIF.

    CLEAR: i_param.
    i_param = 'Safra=' && me->zif_int_acts_tracecotton~at_lote
    && '&CodigoFazenda=' && me->zif_int_acts_tracecotton~at_fazenda
    && '&Lote=' && me->zif_int_acts_tracecotton~at_lgort.

    IF me->zif_int_acts_tracecotton~at_cd_sai IS NOT INITIAL.
      i_param = i_param && '&codigoSai=' && me->zif_int_acts_tracecotton~at_cd_sai.
    ENDIF.


    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url_token    = wa_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url && i_param. "parametros.
*    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa = zif_int_acts_tracecotton=>at_fc_end_point.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    me->zif_int_acts_tracecotton~set_id_referencia( ).
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_fazenda.
    r_if_int_acts_tracecotton = me.
    IF i_fazenda IS NOT INITIAL.
     ME->ZIF_INT_ACTS_TRACECOTTON~at_fazenda = i_fazenda.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_id_referencia.

    "Incluir Chave de Referência
    r_if_int_acts_tracecotton = me.
    me->zif_int_acts_tracecotton~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_lgort.
    r_if_int_acts_tracecotton = me.
    IF i_lgort IS NOT INITIAL.
      me->zif_int_acts_tracecotton~at_lgort = i_lgort.
    ENDIF.
  ENDMETHOD.


METHOD zif_int_acts_tracecotton~set_lote.
  r_if_int_acts_tracecotton = me.
  IF i_lote IS NOT INITIAL.
    me->zif_int_acts_tracecotton~at_lote = i_lote.
  ENDIF.
ENDMETHOD.


  method ZIF_INT_ACTS_TRACECOTTON~SET_SEND_MSG.

     DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INT_ACTS_TRACECOTTON = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.
  endmethod.


  METHOD zif_int_acts_tracecotton~set_servico.
    IF i_servico IS NOT INITIAL.
      zif_int_acts_tracecotton~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_acts_tracecotton~set_tipo.

    IF i_tipo IS NOT INITIAL.
      zif_int_acts_tracecotton~at_tipo = i_tipo.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
