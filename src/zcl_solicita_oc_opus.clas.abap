class ZCL_SOLICITA_OC_OPUS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_SOLICITA_OC_OPUS .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SOLICITA_OC_OPUS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    ME->ZIF_INTEGRACAO_INJECT~AT_ID_INTERFACE    = ZIF_INTEGRACAO=>AT_ID_INTERFACE_SOL_OC_OPUS.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_INTEGRACAO   = ZIF_INTEGRACAO=>AT_TP_INTEGRACAO_OUTBOUND.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_CANAL        = ZIF_INTEGRACAO=>AT_TP_CANAL_COMUNICA_HTTP.
    ME->ZIF_INTEGRACAO_INJECT~AT_TP_SINCRONIA    = ZIF_INTEGRACAO=>AT_TP_SINCRONIA_SINCRONA.
    ME->ZIF_INTEGRACAO_INJECT~AT_AUTENTICA_OPUS  = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_OPUS_SIM.
    ME->ZIF_INTEGRACAO_INJECT~AT_SEND_AUTENTICAO = ZIF_INTEGRACAO=>AT_ID_INTERFACE_AUT_SEND_SIM.
    ME->ZIF_SOLICITA_OC_OPUS~SET_DS_URL( ).

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    R_IF_INTEGRACAO_INJECT = ME.

*    SELECT SINGLE * INTO @DATA(WA_VIAGEM)
*      FROM ZLEST0185
*     WHERE VIAGEM_ID EQ @I_INTEGRACAO-ID_REFERENCIA.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGID
*                            MSGNO = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGNO
*                            ATTR1 = CONV #( I_INTEGRACAO-ID_REFERENCIA ) )
*          MSGID  = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGID
*          MSGNO  = ZCX_INTEGRACAO=>ZCX_ID_INTEGRACAO_NAO_ECONTRAD-MSGNO
*          MSGTY  = 'E'
*          MSGV1  = CONV #( I_INTEGRACAO-ID_REFERENCIA ).
*    ENDIF.

    TRY .
        CAST ZCL_INTEGRACAO_TOKEN(
               ZCL_INTEGRACAO_TOKEN=>ZIF_INTEGRACAO_TOKEN~GET_INSTANCE(
                 )->SET_EMPRESA_TOKEN( EXPORTING I_BUKRS = ME->ZIF_SOLICITA_OC_OPUS~AT_VIAGEM-BUKRS
                 )
             )->ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP(
          IMPORTING
            E_HEADER_FIELDS = DATA(E_HEADER_FIELDS) ).

        ME->ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP( I_HEADER_FIELDS = E_HEADER_FIELDS ).

      CATCH ZCX_ERROR INTO DATA(EX_ERRO).

        RAISE EXCEPTION TYPE ZCX_INTEGRACAO
          EXPORTING
            TEXTID = VALUE #( MSGID = EX_ERRO->ZIF_ERROR~MSGID
                              MSGNO = EX_ERRO->ZIF_ERROR~MSGNO
                              ATTR1 = CONV #( EX_ERRO->ZIF_ERROR~MSGV1 )
                              ATTR2 = CONV #( EX_ERRO->ZIF_ERROR~MSGV2 )
                              ATTR3 = CONV #( EX_ERRO->ZIF_ERROR~MSGV3 )
                              ATTR4 = CONV #( EX_ERRO->ZIF_ERROR~MSGV4 ) )
            MSGID  = EX_ERRO->ZIF_ERROR~MSGID
            MSGNO  = EX_ERRO->ZIF_ERROR~MSGNO
            MSGTY  = 'E'
            MSGV1  = EX_ERRO->ZIF_ERROR~MSGV1
            MSGV2  = EX_ERRO->ZIF_ERROR~MSGV2
            MSGV3  = EX_ERRO->ZIF_ERROR~MSGV3
            MSGV4  = EX_ERRO->ZIF_ERROR~MSGV4.

    ENDTRY.

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


  METHOD ZIF_SOLICITA_OC_OPUS~GET_ID_REFERENCIA.

    R_IF_SOLICITA_OC_OPUS = ME.

    E_REFERENCIA-TP_REFERENCIA = 'CARGUERO_ORDEM_CARRE_OPUS'.
    E_REFERENCIA-ID_REFERENCIA = ME->ZIF_SOLICITA_OC_OPUS~AT_VIAGEM-VIAGEM_ID.

  ENDMETHOD.


  METHOD ZIF_SOLICITA_OC_OPUS~GET_INSTANCE.

    IF ZIF_SOLICITA_OC_OPUS~AT_IF_SOLICITA_OC_OPUS IS NOT BOUND.
      CREATE OBJECT ZIF_SOLICITA_OC_OPUS~AT_IF_SOLICITA_OC_OPUS TYPE ZCL_SOLICITA_OC_OPUS.
    ENDIF.
    R_IF_SOLICITA_OC_OPUS = ZIF_SOLICITA_OC_OPUS~AT_IF_SOLICITA_OC_OPUS.

  ENDMETHOD.


  METHOD ZIF_SOLICITA_OC_OPUS~GET_JSON.

    R_IF_SOLICITA_OC_OPUS = ME.

    TRY .
        E_JSON = ZCL_FMCALL_BASE=>ABAP2JSON( ABAP_DATA = ME->ZIF_SOLICITA_OC_OPUS~AT_OC_SAP_TO_PUS_OV ).
      CATCH CX_ROOT INTO DATA(EX_ROOT).
        ZCX_INTEGRACAO=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = EX_ROOT->GET_LONGTEXT( ) ).
    ENDTRY.

  ENDMETHOD.


  method ZIF_SOLICITA_OC_OPUS~SET_DS_DATA.

    "Incluir Texto JSON para integração
    R_IF_SOLICITA_OC_OPUS = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_BODY = I_JSON.

  endmethod.


  METHOD ZIF_SOLICITA_OC_OPUS~SET_DS_URL.

    R_IF_SOLICITA_OC_OPUS = ME.

    SELECT SINGLE * INTO @DATA(WA_WEBSERVICE)
      FROM ZCIOT_WEBSERVICE
     WHERE TIPO    EQ 'O'
       AND SERVICO EQ '01'.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_INTEGRACAO
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
                            MSGNO = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
                            ATTR1 = 'O'
                            ATTR2 = '01' )
          MSGID  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGID
          MSGNO  = ZCX_INTEGRACAO=>ZCX_SERVICO_HTTP_CONFIG-MSGNO
          MSGTY  = 'E'
          MSGV1  = 'O'
          MSGV2  = '01'.
    ENDIF.

    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FORMATO          = 'JSON'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_CONTENT_TYPE     = WA_WEBSERVICE-CONTENT_TYPE.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL_TOKEN        = WA_WEBSERVICE-URL_TOKEN.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_URL              = WA_WEBSERVICE-URL && 'ordemcarregamento'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_FUNCAO_PROCESSA  = ZIF_SOLICITA_OC_OPUS=>AT_FUNCAO_SOLICITA_OC_OPUS.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO           = 'POST'.
    ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_SERVER_PROTOCOLO = ''.

    ME->ZIF_SOLICITA_OC_OPUS~SET_ID_REFERENCIA( ).

  ENDMETHOD.


  METHOD ZIF_SOLICITA_OC_OPUS~SET_ID_REFERENCIA.

    "Incluir Chave de Referência
    R_IF_SOLICITA_OC_OPUS = ME.
    ME->ZIF_SOLICITA_OC_OPUS~GET_ID_REFERENCIA( IMPORTING E_REFERENCIA = ME->ZIF_INTEGRACAO_INJECT~AT_REFERENCIA ).

  ENDMETHOD.


  METHOD ZIF_SOLICITA_OC_OPUS~SET_SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_SOLICITA_OC_OPUS = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  ENDMETHOD.


  METHOD zif_solicita_oc_opus~set_solicita_ordem_carrega.

    DATA: lc_out_od     TYPE zde_resp_ord_carregamento,
          lc_zsdt0001od TYPE zsdt0001od.

    r_if_solicita_oc_opus = me.
    me->zif_solicita_oc_opus~at_viagem = i_zlest0185.
    me->zif_solicita_oc_opus~at_oc_sap_to_pus_ov = i_zde_json_oc_sap_to_pus_ov.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_solicita_oc_opus~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_id_referencia(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao = e_integracao
      ).

    CLEAR: lc_zsdt0001od.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_out_od ).

    lc_zsdt0001od-id_ordem           = zcl_string=>lpad( i_str = lc_out_od-idordem     i_qtd = 10 i_char = '0' ).
    lc_zsdt0001od-nr_ordem           = zcl_string=>lpad( i_str = lc_out_od-numeroordem i_qtd = 09 i_char = '0' ).
    lc_zsdt0001od-dt_emissao         = zcl_string=>replace( i_str = i_zde_json_oc_sap_to_pus_ov-dataemissao  i_char_old = '-' ).
    lc_zsdt0001od-dt_validade        = zcl_string=>replace( i_str = i_zde_json_oc_sap_to_pus_ov-datavalidade i_char_old = '-' ).
    lc_zsdt0001od-nr_safra           = i_zde_json_oc_sap_to_pus_ov-safra.
    lc_zsdt0001od-id_bukrs           = i_zde_json_oc_sap_to_pus_ov-empresa.
    lc_zsdt0001od-id_branch          = i_zde_json_oc_sap_to_pus_ov-filialresponsavel.
    lc_zsdt0001od-id_bukrs_ag        = i_zde_json_oc_sap_to_pus_ov-empresa.
    lc_zsdt0001od-id_branch_ag       = i_zde_json_oc_sap_to_pus_ov-filialemissora.
    lc_zsdt0001od-id_local_coleta    = i_zde_json_oc_sap_to_pus_ov-fornecedorlocalcoleta.
    lc_zsdt0001od-id_local_destino   = i_zde_json_oc_sap_to_pus_ov-fornecedorlocaldestino.
    lc_zsdt0001od-id_local_descarga  = i_zde_json_oc_sap_to_pus_ov-clientelocaldescarga.
    lc_zsdt0001od-id_produto         = i_zde_json_oc_sap_to_pus_ov-numeromaterialsap.
    lc_zsdt0001od-id_motorista       = i_zde_json_oc_sap_to_pus_ov-motorista-id_fornecedor.
    lc_zsdt0001od-ds_placa_trator    = i_zde_json_oc_sap_to_pus_ov-cavalo-pc_veiculo.
    lc_zsdt0001od-ds_placa_reboq_1   = i_zde_json_oc_sap_to_pus_ov-carreta1-pc_veiculo.
    lc_zsdt0001od-ds_placa_reboq_2   = i_zde_json_oc_sap_to_pus_ov-carreta2-pc_veiculo.
    lc_zsdt0001od-ds_placa_reboq_3   = i_zde_json_oc_sap_to_pus_ov-carreta3-pc_veiculo.
*-CS2021000253-26.04.2024-#59941-JT-inicio
    lc_zsdt0001od-agente_frete       = i_zde_json_oc_sap_to_pus_ov-transportadoraterceiro-id_fornecedor.
*-CS2021000253-26.04.2024-#59941-JT-fim
    lc_zsdt0001od-tp_status          = lc_out_od-status.
    lc_zsdt0001od-nr_peso_alvo       = i_zde_json_oc_sap_to_pus_ov-numeropesoalvo.
    lc_zsdt0001od-nr_frete_comb      = i_zde_json_oc_sap_to_pus_ov-numerofretecombinado.
    lc_zsdt0001od-ds_url_file        = lc_out_od-url.
    MODIFY zsdt0001od FROM lc_zsdt0001od.
    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
