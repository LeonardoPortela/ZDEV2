*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZCL_INT_OB_CRIA_PED_SAIDA_LUFT                                                       *
*& Chamado        : USER STORY 172200                                                                    *
*& Data           : 01/04/2025                                                                           *
*& Especificado   : Wellington Pereira                                                                   *
*& Desenvolvimento: Paulo Ferraz                                                                         *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 01/04/2025  |DEVK9A2HOS  |PAFERRAZ       |Integração LUFT - Receb. Produtos carregado (Pedido Saída). *
*&                                          |Desenvolvimento inicial. Chamado: 172200.                   *
*--------------------------------------------------------------------------------------------------------*
*& 01/04/2025  |DEVK9A2HOS  |NSEGATIN       |Integração LUFT - Receb. Produtos carregado (Pedido Saída). *
*&                                          |Ajuste do Desenvolv. inicial após adequação da Especificação*
*&                                          |Chamado: 172200.                                            *
*--------------------------------------------------------------------------------------------------------*
class ZCL_INT_IB_CARREGAMENTO_CDLUFT definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '303' ##NO_TEXT.
  data AT_RECB_PROD_CARREG type ZSDE_INT_LUFT_RECB_PROD_CARREG .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_INT_IB_CARREGAMENTO_CDLUFT IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~configure_server.

    DATA: lva_reason TYPE string,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          output = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code        = lva_code
        IMPORTING
          e_desc_status = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = CONV #( lva_code )
          reason = CONV #( lva_reason )
       ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inbound~processar_requisicao.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    DATA: lwa_bordero TYPE zsde_int_luft_recb_prod_carreg.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    IF me->zif_integracao_inject~at_info_request_http-ds_body  IS NOT INITIAL AND lwa_bordero IS INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = me->zif_integracao_inject~at_info_request_http-ds_body CHANGING data = lwa_bordero ).
    ENDIF.

    CREATE OBJECT lc_integracao.

    me->zif_integracao_inject~at_referencia-tp_referencia = 'BORDERO_SAIDA'.
    me->zif_integracao_inject~at_referencia-id_referencia = lwa_bordero-identificador.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.


    DATA: lva_seq_autorizacao TYPE zsdt0133-seq_autorizacao_embarque.
    DATA: lva_nro_cg          TYPE zsdt0133-nro_cg.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.

    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro    = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.

    ENDIF.

    IF at_recb_prod_carreg IS INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = at_recb_prod_carreg ).

    ENDIF.

    IF zcl_util_sd=>ck_integration_luft_active( i_direcao = 'S' ) EQ abap_false.
      r_msg_erro = 'Integração com LUFT Logistics desabilitada!'.
      RETURN.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF at_recb_prod_carreg-identificador IS INITIAL .
      r_msg_erro = 'Obrigatório informar Id Autorização!'.
      RETURN.
    ENDIF.

    SPLIT at_recb_prod_carreg-identificador AT '_' INTO lva_nro_cg lva_seq_autorizacao.

    IF lva_nro_cg IS INITIAL OR lva_seq_autorizacao IS INITIAL.
      r_msg_erro = 'Identificador da autorização de embarque em formato inválido! Deve conter separador _ '.
      RETURN.
    ENDIF.

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga_single        = CONV #( lva_nro_cg )
      IMPORTING
        e_cargas                 = DATA(lit_carga) ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    IF sy-subrc NE 0 OR lit_carga[] IS INITIAL.
      r_msg_erro = |Carga { lva_nro_cg } não encontrada!|.
      RETURN.
    ENDIF.

    IF lwa_carga-carga_conferida EQ abap_true.
      r_msg_erro = |Carga { lva_nro_cg } já foi conferida!|.
      RETURN.
    ENDIF.

    IF lwa_carga-dt_autorizacao_embarque IS INITIAL.
      r_msg_erro = |Carga { lva_nro_cg } não possui autorização de embarque ativa! Aguardar uma nova autorização!|.
      RETURN.
    ENDIF.

    IF lwa_carga-nro_pedido_luft IS INITIAL.
      r_msg_erro = |Carga { lva_nro_cg } sem pedido LUFT criado!|.
      RETURN.
    ENDIF.

    IF lwa_carga-seq_autorizacao_embarque NE lva_seq_autorizacao.
      DATA(lva_ultimo_pedido) =  lwa_carga-nro_cg && '_' && lwa_carga-seq_autorizacao_embarque.
      r_msg_erro = |Pedido desatualizado para essa Carregamento! Pedido que deve ser utilizado é o { lva_ultimo_pedido } |.
      RETURN.
    ENDIF.

    LOOP AT at_recb_prod_carreg-pedido-listaitenspedido INTO DATA(el_lst_pedido).

      IF el_lst_pedido-linha IS INITIAL.
        r_msg_erro = 'Obrigatório informar Items!'.
        RETURN.

      ENDIF.

      IF el_lst_pedido-codigoproduto IS INITIAL.
        r_msg_erro = 'Obrigatório informar o Código do Produto!'.
        RETURN.

      ENDIF.

      IF el_lst_pedido-descricao IS INITIAL .
        r_msg_erro = 'Obrigatório informar Descrição prod.!'.
        RETURN.

      ENDIF.

      IF el_lst_pedido-lote IS INITIAL .
        r_msg_erro = 'Obrigatório informar Lote!'.
        RETURN.

      ENDIF.

      IF el_lst_pedido-quantidade IS INITIAL .
        r_msg_erro = 'Obrigatório informar Qtd!'.
        RETURN.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lt_saida TYPE TABLE OF zsdt0376.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound       IS NOT INITIAL AND
       at_recb_prod_carreg IS INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = at_recb_prod_carreg ).

    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound = i_msg_inbound
                                                      IMPORTING e_status_code  = DATA(_status_code)
                                                      RECEIVING r_msg_erro     = e_msg_erro
                                                     ).

    IF e_msg_erro IS INITIAL.
      SPLIT at_recb_prod_carreg-identificador AT '_' INTO DATA(lva_nro_cg) DATA(lva_seq_autorizacao).

      IF lva_nro_cg IS INITIAL OR lva_seq_autorizacao IS INITIAL.
        e_msg_erro = 'Identificador da autorização de embarque em formato inválido! Deve conter separador _ '.
      ENDIF.
    ENDIF.

    IF e_msg_erro IS NOT INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request

      ENDIF.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.

    ENDIF.

    DELETE FROM zsdt0376 WHERE id_autorizacao_embarque EQ lva_nro_cg.

    LOOP AT at_recb_prod_carreg-pedido-listaitenspedido INTO DATA(el_lst_pedido).
      APPEND INITIAL LINE TO lt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
      <fs_saida>-id_autorizacao_embarque = lva_nro_cg.
      <fs_saida>-id_item                 = el_lst_pedido-linha.
      <fs_saida>-lote_fornecedor         = el_lst_pedido-lote.
      <fs_saida>-matnr                   = el_lst_pedido-codigoproduto.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fs_saida>-matnr
        IMPORTING
          output = <fs_saida>-matnr.

      SELECT SINGLE charg
        FROM mch1 INTO <fs_saida>-lote
       WHERE matnr EQ <fs_saida>-matnr
         AND licha EQ <fs_saida>-lote_fornecedor.

      <fs_saida>-descricacao_produto     = el_lst_pedido-descricao.
      <fs_saida>-quantidade              = el_lst_pedido-quantidade.
      <fs_saida>-date_create             = sy-datlo.
      <fs_saida>-time_create             = sy-timlo.
      <fs_saida>-user_create             = sy-uname.

    ENDLOOP.

    MODIFY zsdt0376 FROM TABLE lt_saida.

    IF sy-subrc IS INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '200'.
      e_msg_erro     = 'Ok'.
      e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                   &&  '" '  && cl_abap_char_utilities=>newline &&
                       ' }'.

    ELSE.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_erro     = 'Erro na atualização dos dados da estrutura Receb. Produtos Carregados'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
