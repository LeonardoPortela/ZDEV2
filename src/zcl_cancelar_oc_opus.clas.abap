class ZCL_CANCELAR_OC_OPUS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_CANCELAR_OC_OPUS .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CANCELAR_OC_OPUS IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_can_oc_opus.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_sim.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_CANCELAR_OC_OPUS~GET_ID_REFERENCIA.

    R_IF_CANCELAR_OC_OPUS = ME.

    IF ME->ZIF_CANCELAR_OC_OPUS~AT_VIAGEM IS NOT INITIAL.
      E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_ORDEM_CARRE_OPUS_CANC'.
      E_REFERENCIA-ID_REFERENCIA = ME->ZIF_CANCELAR_OC_OPUS~AT_VIAGEM-VIAGEM_ID.
    ENDIF.

  endmethod.


  method ZIF_CANCELAR_OC_OPUS~GET_INSTANCE.

    IF ZIF_CANCELAR_OC_OPUS~AT_IF_CANCELAR_OC_OPUS IS NOT BOUND.
      CREATE OBJECT ZIF_CANCELAR_OC_OPUS~AT_IF_CANCELAR_OC_OPUS TYPE ZCL_CANCELAR_OC_OPUS.
    ENDIF.

    R_IF_CANCELAR_OC_OPUS = ZIF_CANCELAR_OC_OPUS~AT_IF_CANCELAR_OC_OPUS.


  endmethod.


  method ZIF_CANCELAR_OC_OPUS~SET_CANCELAR_ORDEM_CARREGA.

    R_IF_CANCELAR_OC_OPUS = ME.

    E_CANCELADA = ABAP_FALSE.

    ME->ZIF_CANCELAR_OC_OPUS~AT_VIAGEM = I_ZLEST0185.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_CANCELAR_OC_OPUS~SET_DS_URL(
      )->SET_ID_REFERENCIA(
      )->SET_SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO E_INTEGRACAO = E_INTEGRACAO
      ).

    IF E_INTEGRACAO-DS_DATA_RETORNO EQ ZIF_CANCELAR_OC_OPUS=>AT_SUCESS_CANCELAMENTO.
      E_CANCELADA = ABAP_TRUE.
    ENDIF.

  endmethod.


  method ZIF_CANCELAR_OC_OPUS~SET_DS_URL.

    R_IF_CANCELAR_OC_OPUS = ME.

    SELECT SINGLE * INTO @DATA(WA_WEBSERVICE)
      FROM ZCIOT_WEBSERVICE
     WHERE TIPO    EQ 'O'
       AND SERVICO EQ 'CO'.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                            ATTR1 = 'O'
                            ATTR2 = 'CO' )
          MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
          MSGTY  = 'E'
          MSGV1  = 'O'
          MSGV2  = 'CO'.
    ENDIF.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE     = WA_WEBSERVICE-CONTENT_TYPE.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL              = WA_WEBSERVICE-URL && ME->ZIF_CANCELAR_OC_OPUS~AT_ORDEM_CARREGAMENTO-ID_ORDEM.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = ZIF_CANCELAR_OC_OPUS=>AT_FUNCAO_CANCELAR_OC_OPUS.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO           = 'POST'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.


  endmethod.


  method ZIF_CANCELAR_OC_OPUS~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_CANCELAR_OC_OPUS = ME.

    ME->ZIF_CANCELAR_OC_OPUS~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).


  endmethod.


  method ZIF_CANCELAR_OC_OPUS~SET_ORDEM_CARREGAMENTO.

    R_IF_CANCELAR_OC_OPUS = ME.

    SELECT SINGLE * INTO ME->ZIF_CANCELAR_OC_OPUS~AT_ORDEM_CARREGAMENTO
      FROM ZSDT0001OD
     WHERE ID_ORDEM EQ I_ID_ORDEM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ORDEM_CAR_NOT_EXISTS-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_ORDEM_CAR_NOT_EXISTS-MSGNO
                            ATTR1 = CONV #( I_ID_ORDEM )
                           )
          MSGID  = ZCX_INTEGRACAO=>ZCX_ORDEM_CAR_NOT_EXISTS-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_ORDEM_CAR_NOT_EXISTS-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_ID_ORDEM ).
    ENDIF.


  endmethod.


  method ZIF_CANCELAR_OC_OPUS~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_CANCELAR_OC_OPUS = ME.

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


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.
ENDCLASS.
