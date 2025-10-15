class ZCL_INT_IB_PARAM_EMP_CLASS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '103' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_PARAM_EMP_CLASS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

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

    DATA: lit_data_inbound  TYPE zsdt0001ce_t.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST'   AND
       me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'DELETE'.

      r_msg_erro = 'Metodo informado não reconhecido!'.
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_data_inbound ).

    LOOP AT lit_data_inbound INTO DATA(lwa_data).

      DATA(_tabix) = sy-tabix.

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*

      "Campos obrigatorios em operação de Criação/Atualização/Delete
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST OR C_DELETE.

          IF lwa_data-lifnr IS INITIAL.
            r_msg_erro = 'Codigo Fornecedor não foi informado!' && ' Index Objeto: ' && _tabix.
            RETURN.
          ENDIF.

      ENDCASE.

      "Campos obrigatorios em operação de Criação/Atualização
      CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
        WHEN C_POST.


      ENDCASE.

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Regras de Negocio
*-----------------------------------------------------------------------------------------------------------------------*

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


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: lit_data_inbound  TYPE zsdt0001ce_t.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lit_data_inbound ).
    ENDIF.

    e_msg_erro = ME->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).
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
            FROM zsdt0001ce INTO @DATA(lwa_zsdt0001ce)
           WHERE lifnr       EQ @lwa_data_inbound-lifnr.

          IF SY-SUBRC EQ 0.
            lwa_data_inbound-dt_registro = lwa_zsdt0001ce-dt_registro.
            lwa_data_inbound-hr_registro = lwa_zsdt0001ce-hr_registro.
          ELSE.
            lwa_data_inbound-dt_registro = sy-datum.
            lwa_data_inbound-hr_registro = sy-uzeit.
          ENDIF.

          lwa_data_inbound-id_integracao = ME->zif_integracao_inbound~at_id_integracao.  "Set Protocolo Integração
          MODIFY ZSDT0001ce FROM lwa_data_inbound.

          IF SY-SUBRC EQ 0.
            _change_bd = abap_true.
          ELSE.
            e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
            EXIT.
          ENDIF.

        WHEN 'DELETE'.

          DELETE FROM ZSDT0001ce
           WHERE lifnr       EQ lwa_data_inbound-lifnr.

          SELECT SINGLE *
            FROM ZSDT0001ce INTO lwa_zsdt0001ce
           WHERE lifnr       EQ lwa_data_inbound-lifnr.

          IF SY-SUBRC NE 0.
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
      e_msg_outbound = ' { "protocolo" : "'   && ME->zif_integracao_inbound~at_id_integracao &&  '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code                                   &&  '" '  && cl_abap_char_utilities=>newline &&
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
