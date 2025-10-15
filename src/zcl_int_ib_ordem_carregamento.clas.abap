class ZCL_INT_IB_ORDEM_CARREGAMENTO definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_ORDEM_CARREGAMENTO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
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


  method ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.


    CONSTANTS: C_DELETE TYPE C LENGTH 50 VALUE 'DELETE',
               C_POST   TYPE C LENGTH 50 VALUE 'POST'.

    DATA: lit_data_inbound  TYPE zsdt0001od_t.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST'   AND
       me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'DELETE'.

      r_msg_erro = 'Metodo informado não reconhecido!'.
      RETURN.

    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_data_inbound ).

    LOOP AT lit_data_inbound INTO DATA(lwa_data_ordem).

      DATA(_tabix) = sy-tabix.

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*

      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST OR C_DELETE.
          IF lwa_data_ordem-id_ordem IS INITIAL.
            r_msg_erro = 'Id. da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.
      ENDCASE.

      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.

          IF lwa_data_ordem-nr_ordem IS INITIAL.
            r_msg_erro = 'Nro. da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-dt_emissao IS INITIAL.
            r_msg_erro = 'Data Emissão da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-dt_validade IS INITIAL.
            r_msg_erro = 'Data Validade da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-nr_safra IS INITIAL.
            r_msg_erro = 'Safra da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_bukrs IS INITIAL.
            r_msg_erro = 'Empresa da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_branch IS INITIAL.
            r_msg_erro = 'Filial da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_bukrs_ag IS INITIAL.
            r_msg_erro = 'Empresa Agente Frete da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_branch_ag IS INITIAL.
            r_msg_erro = 'Filial Agente Frete da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_local_coleta IS INITIAL.
            r_msg_erro = 'Local Coleta da Ordem de Carregamento não foi informado!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_local_destino IS INITIAL.
            r_msg_erro = 'Local Destino da Ordem de Carregamento não foi informado!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_local_descarga IS INITIAL.
            r_msg_erro = 'Local Descarga da Ordem de Carregamento não foi informado!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_produto IS INITIAL.
            r_msg_erro = 'Produto da Ordem de Carregamento não foi informado!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-id_motorista IS INITIAL.
            r_msg_erro = 'Motorista da Ordem de Carregamento não foi informado!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-ds_placa_trator IS INITIAL.
            r_msg_erro = 'Placa Trator da Ordem de Carregamento não foi informada!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

          IF lwa_data_ordem-tp_status IS INITIAL.
            r_msg_erro = 'Status da Ordem de Carregamento não foi informado!' && 'Index Objeto: ' && _tabix && '-[ID]' && lwa_data_ordem-id_ordem .
            RETURN.
          ENDIF.

      ENDCASE.

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Regras de Negocio
*-----------------------------------------------------------------------------------------------------------------------*
      SELECT SINGLE *
        FROM zsdt0001od INTO @DATA(lwa_zsdt0001od)
       WHERE id_ordem EQ @lwa_data_ordem-id_ordem.

      IF SY-SUBRC EQ 0.
        IF lwa_zsdt0001od-tp_status = 'FE'.
          r_msg_erro = 'Ordem Carregamento com Id:' && lwa_data_ordem-id_ordem && ', encontra-se fechada no SAP!' && '-[ID]' && lwa_data_ordem-id_ordem .
          RETURN.
        ENDIF.
      ENDIF.

      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo..
        WHEN C_POST.

        WHEN C_DELETE.

      ENDCASE.

    ENDLOOP.



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


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lit_data_inbound TYPE zsdt0001od_t,
          lva_operacao     TYPE string,
          lva_ds_erro      TYPE string.


    r_if_integracao_inject = me.
    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lit_data_inbound ).
    ENDIF.

    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).
    IF e_msg_erro IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    DATA(_change_bd) = abap_false.

    LOOP AT lit_data_inbound INTO DATA(lwa_data_inbound).

      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN 'POST'. "Inclusão/Modificação

          SELECT SINGLE *
            FROM zsdt0001od INTO @DATA(lwa_zsdt0001od)
           WHERE id_ordem EQ @lwa_data_inbound-id_ordem.

          IF sy-subrc EQ 0.
            lwa_data_inbound-dt_registro  = lwa_zsdt0001od-dt_registro.
            lwa_data_inbound-hr_registro  = lwa_zsdt0001od-hr_registro.
            lwa_data_inbound-ds_url_file  = lwa_zsdt0001od-ds_url_file.
            lwa_data_inbound-agente_frete = lwa_zsdt0001od-agente_frete. "*-CS2021000253-26.04.2024-#59941-JT
            lwa_data_inbound-dt_update    = sy-datum.
            lwa_data_inbound-hr_update    = sy-uzeit.
          ELSE.
*-CS2021000253-26.04.2024-#59941-JT-inicio
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lwa_data_inbound-id_branch_ag
              IMPORTING
                output = lwa_data_inbound-agente_frete.
*-CS2021000253-26.04.2024-#59941-JT-fim

            lwa_data_inbound-dt_registro  = sy-datum.
            lwa_data_inbound-hr_registro  = sy-uzeit.
          ENDIF.

          lwa_data_inbound-id_integracao  = me->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
          MODIFY zsdt0001od FROM lwa_data_inbound.

          IF sy-subrc EQ 0.
            _change_bd = abap_true.
          ELSE.
            e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
            EXIT.
          ENDIF.

        WHEN 'DELETE'.

          DELETE FROM zsdt0001od WHERE id_ordem EQ lwa_data_inbound-id_ordem.

          SELECT SINGLE *
            FROM zsdt0001od INTO lwa_zsdt0001od
           WHERE id_ordem EQ lwa_data_inbound-id_ordem.

          IF sy-subrc NE 0.
            _change_bd = abap_true.
          ELSE.
            e_msg_erro = 'Houve um erro ao excluir o registro no banco de dados!'.
            EXIT.
          ENDIF.

        WHEN OTHERS.
          e_msg_erro = 'Operação não prevista!'.
          EXIT.
      ENDCASE.

      IF e_msg_erro IS NOT INITIAL.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF ( _change_bd = abap_true ) AND ( e_msg_erro IS INITIAL ).

      COMMIT WORK.

      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.
      e_msg_outbound = ' { "protocolo" : "'   && me->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                           &&  '" '  && cl_abap_char_utilities=>newline &&
                       ' }'.
    ELSE.

      ROLLBACK WORK.

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


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
