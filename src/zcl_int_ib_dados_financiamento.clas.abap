CLASS zcl_int_ib_dados_financiamento DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_zfit0132,
        id_empresa_sap           TYPE zfit0132-id_empresa_sap,
        data_baixa               TYPE char10,
        doc_sap_origem           TYPE zfit0132-doc_sap_origem,
        doc_sap_pagamento_origem TYPE zfit0132-doc_sap_pagamento_origem,
        doc_sap_baixa            TYPE zfit0132-doc_sap_baixa,
        doc_sap_compensacao      TYPE zfit0132-doc_sap_compensacao,
        contrato_numero          TYPE zfit0132-contrato_numero,
        data_origem              TYPE char10,
        id_sistema               TYPE zfit0132-id_sistema,
        id_lancamento            TYPE zfit0132-id_lancamento,
        operacao_descricao       TYPE zfit0132-operacao_descricao,
        valor_original_d         TYPE zfit0132-valor_original_d,
        valor_original_r         TYPE zfit0132-valor_original_r,
        vr_juros_real            TYPE zfit0132-vr_juros_real,
        vr_juros_dolar           TYPE zfit0132-vr_juros_dolar,
        vr_correcao_real         TYPE zfit0132-vr_correcao_real,
        valor_baixa_real         TYPE zfit0132-valor_baixa_real,
        valor_baixa_dolar        TYPE zfit0132-valor_baixa_dolar,
        vr_cotacao_origem        TYPE zfit0132-vr_cotacao_origem,
        vr_cotacao_baixa         TYPE zfit0132-vr_cotacao_baixa,
        id_filial_sap            TYPE zfit0132-id_filial_sap,
        parceiro_id              TYPE zfit0132-parceiro_id,
        documento_sap_juros      TYPE zfit0132-documento_sap_juros,
        documento_sap_correcao   TYPE zfit0132-documento_sap_correcao,
      END OF ty_zfit0132,

      BEGIN OF ty_xrt,
        chave_referencia   TYPE zfit0133-chave_referencia,
        mvt_contador       TYPE zfit0133-mvt_contador,
        contrato_numero    TYPE zfit0133-contrato_numero,
        data_origem        TYPE char10,
        id_sistema         TYPE zfit0133-id_sistema,
        operacao_id        TYPE zfit0133-operacao_id,
        operacao_descricao TYPE zfit0133-operacao_descricao,
        data_baixa         TYPE char10,
        tipo               TYPE zfit0133-tipo,
        credito_debito     TYPE zfit0133-credito_debito,
        apropriacao_caixa  TYPE zfit0133-apropriacao_caixa,
        valor_original_d   TYPE zfit0133-valor_original_d,
        valor_original_r   TYPE zfit0133-valor_original_r,
        vr_cotacao         TYPE zfit0133-vr_cotacao,
        id_empresa_sap     TYPE zfit0133-id_empresa_sap,
        id_filial_sap      TYPE zfit0133-id_filial_sap,
        parceiro_id        TYPE zfit0133-chave_referencia,
        conta_contabil     TYPE zfit0133-mvt_contador,
        par_contabil       TYPE zfit0133-contrato_numero,
      END OF ty_xrt,

      BEGIN OF ty_xrt_parcelas,
        contrato_numero TYPE zfit0134-contrato_numero,
        ent_sai         TYPE zfit0134-ent_sai,
        data_efetiva    TYPE char10,
        valor_valido    TYPE zfit0134-valor_valido,
        valor_pago      TYPE zfit0134-valor_pago,
        taxa            TYPE zfit0134-taxa,
      END OF ty_xrt_parcelas.

    DATA:
      BEGIN OF zde_data_request ,
        data_ini       TYPE char10,
        data_fim       TYPE char10,
        sigam_produtor TYPE TABLE OF ty_zfit0132,
        xrt            TYPE TABLE OF ty_xrt,
        xrt_parcelas   TYPE TABLE OF ty_xrt_parcelas,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        grupos TYPE TABLE OF t024,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '144' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_DADOS_FINANCIAMENTO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

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

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: lwa_data_request LIKE zde_data_request.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*

    DELETE lwa_data_request-sigam_produtor WHERE id_empresa_sap IS INITIAL.
    DELETE lwa_data_request-xrt            WHERE chave_referencia IS INITIAL.
    DELETE lwa_data_request-xrt_parcelas   WHERE contrato_numero IS INITIAL.

    IF lwa_data_request-data_ini IS INITIAL or lwa_data_request-data_fim IS INITIAL.
      r_msg_erro = 'Favor informar a data inicial e final!'.
      RETURN.
    ELSEIF lwa_data_request-sigam_produtor IS INITIAL AND lwa_data_request-xrt IS INITIAL AND lwa_data_request-xrt_parcelas IS NOT INITIAL.
      r_msg_erro = 'Favor informar dados em ao menos uma estrutura'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    TYPES:
      BEGIN OF ty_referencia,
        referencia TYPE zch_ref,
      END OF ty_referencia,

      BEGIN OF ty_remessa,
        remessa TYPE vbeln,
      END OF ty_remessa.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response,
          lv_ok1            TYPE flag VALUE abap_true,
          lv_ok2            TYPE flag VALUE abap_true,
          lv_ok3            TYPE flag VALUE abap_true,
          lv_data_ini       TYPE datum,
          lv_data_fim       TYPE datum,
          lt_zfit0132       TYPE TABLE OF zfit0132,
          lt_zfit0133       TYPE TABLE OF zfit0133,
          lt_zfit0134       TYPE TABLE OF zfit0134.

    DATA: lra_ekgrp TYPE RANGE OF ekgrp,
          lra_eknam TYPE RANGE OF t024-eknam.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  DATA(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

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

    DELETE lwa_data_request-sigam_produtor WHERE id_empresa_sap IS INITIAL.
    DELETE lwa_data_request-xrt            WHERE chave_referencia IS INITIAL.
    DELETE lwa_data_request-xrt_parcelas   WHERE contrato_numero IS INITIAL.

    REPLACE ALL OCCURRENCES OF '-' IN lwa_data_request-data_ini WITH ''.
    CONDENSE lwa_data_request-data_ini NO-GAPS.
    lv_data_ini = lwa_data_request-data_ini.

    REPLACE ALL OCCURRENCES OF '-' IN lwa_data_request-data_fim WITH ''.
    CONDENSE lwa_data_request-data_fim NO-GAPS.
    lv_data_fim = lwa_data_request-data_fim.

    IF lwa_data_request-sigam_produtor IS NOT INITIAL.

      DELETE FROM zfit0132
      WHERE data_baixa BETWEEN lv_data_ini AND lv_data_fim.

      LOOP AT lwa_data_request-sigam_produtor ASSIGNING FIELD-SYMBOL(<fs_sigam>).
        APPEND INITIAL LINE TO lt_zfit0132 ASSIGNING FIELD-SYMBOL(<fs_zfit0132>).

        REPLACE ALL OCCURRENCES OF '-' IN <fs_sigam>-data_baixa WITH ''.
        CONDENSE <fs_sigam>-data_baixa NO-GAPS.

        REPLACE ALL OCCURRENCES OF '-' IN <fs_sigam>-data_origem WITH ''.
        CONDENSE <fs_sigam>-data_origem NO-GAPS.

        MOVE-CORRESPONDING <fs_sigam> TO <fs_zfit0132>.
      ENDLOOP.

      MODIFY zfit0132 FROM TABLE lt_zfit0132.
      IF sy-subrc IS NOT INITIAL.
        lv_ok1 = abap_false.
      ENDIF.

    ENDIF.

    IF lwa_data_request-xrt IS NOT INITIAL.

      DELETE FROM zfit0133
      WHERE data_baixa BETWEEN lv_data_ini AND lv_data_fim.

      LOOP AT  lwa_data_request-xrt ASSIGNING FIELD-SYMBOL(<fs_xrt>).

        APPEND INITIAL LINE TO lt_zfit0133 ASSIGNING FIELD-SYMBOL(<fs_zfit0133>).

        REPLACE ALL OCCURRENCES OF '-' IN <fs_xrt>-data_baixa WITH ''.
        CONDENSE <fs_xrt>-data_baixa NO-GAPS.

        REPLACE ALL OCCURRENCES OF '-' IN <fs_xrt>-data_origem WITH ''.
        CONDENSE <fs_xrt>-data_origem NO-GAPS.

        MOVE-CORRESPONDING <fs_xrt> TO <fs_zfit0133>.

      ENDLOOP.

      MODIFY zfit0133 FROM TABLE lt_zfit0133.
      IF sy-subrc IS NOT INITIAL.
        lv_ok2 = abap_false.
      ENDIF.

    ENDIF.

    IF lwa_data_request-xrt_parcelas IS NOT INITIAL.

      DELETE FROM zfit0134
      WHERE data_efetiva BETWEEN lv_data_ini AND lv_data_fim.

      LOOP AT lwa_data_request-xrt_parcelas ASSIGNING FIELD-SYMBOL(<fs_xrt_parcelas>).

        APPEND INITIAL LINE TO lt_zfit0134 ASSIGNING FIELD-SYMBOL(<fs_zfit0134>).

        REPLACE ALL OCCURRENCES OF '-' IN <fs_xrt_parcelas>-data_efetiva WITH ''.
        CONDENSE <fs_xrt_parcelas>-data_efetiva NO-GAPS.

        MOVE-CORRESPONDING <fs_xrt_parcelas> TO <fs_zfit0134>.

      ENDLOOP.

      MODIFY zfit0134 FROM TABLE lt_zfit0134.
      IF sy-subrc IS NOT INITIAL.
        lv_ok3 = abap_false.
      ENDIF.
    ENDIF.

    IF lv_ok1 IS NOT INITIAL AND lv_ok2 IS NOT INITIAL AND lv_ok3 IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '200'.
      e_msg_erro     = 'Ok'.
      e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                   &&  '" '  && cl_abap_char_utilities=>newline &&
                       ' }'.
    ELSE.

      IF lv_ok1 IS INITIAL.
        e_msg_erro = 'Erro na atualização da estrutura SIGAM_PRODUTOR'.
      ENDIF.

      IF lv_ok2 IS INITIAL.
        CONCATENATE e_msg_erro 'Erro na atualização da estrutura XRT' INTO e_msg_erro SEPARATED BY space.
      ENDIF.

      IF lv_ok3 IS INITIAL.
        CONCATENATE e_msg_erro 'Erro na atualização da estrutura XRT_PARCELAS' INTO e_msg_erro SEPARATED BY space.
      ENDIF.

      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.

    ENDIF.

  ENDMETHOD.


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
ENDCLASS.
