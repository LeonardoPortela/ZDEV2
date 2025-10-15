class ZCL_INT_IB_CONSULTA_PARAM_WS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '114' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONSULTA_PARAM_WS IMPLEMENTATION.


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

    DATA: lwa_data_inbound TYPE zde_param_consulta_ws.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_get.
      r_msg_erro = 'Metodo informado não previsto!'.
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_inbound ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF lwa_data_inbound-cd_recurso IS INITIAL.
      r_msg_erro = 'Cd. Recurso é um campo obrigatório!'.
      RETURN.
    ENDIF.

    IF lwa_data_inbound-cd_sistema IS INITIAL.
      r_msg_erro = 'Cd. Sistema é um campo obrigatório!'.
      RETURN.
    ENDIF.


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

    DATA: lwa_data_inbound  TYPE zde_param_consulta_ws,
          lwa_data_retorno  TYPE zde_param_consulta_ws_ret.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_inbound ).
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

    CASE SY-SYSID.
      WHEN 'DEV'.
        DATA(_TP_AMBIENTE) = '01'.
      WHEN 'QAS'.
        _TP_AMBIENTE       = '02'.
      WHEN 'PRD'.
        _TP_AMBIENTE       = '03'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    TRY.
        EXEC SQL.
          OPEN REGISTROS FOR

           SELECT TOP 1 O.DS_DOMINIO ||
                  CASE WHEN TRIM(O.DS_HOST)    <> '' THEN '/' || O.DS_HOST    ELSE '' END||
                  CASE WHEN TRIM(S.DS_SERVICO) <> '' THEN '/' || S.DS_SERVICO ELSE '' END||
                  '/' || S.CD_RECURSO AS URI,
                  U.DS_SENHA_BASE64
             FROM SAPHANADB.ZWST0003 O,
                  SAPHANADB.ZWST0002 S,
                  SAPHANADB.ZWAT0001 U
            WHERE O.MANDT       = :SY-MANDT
              AND O.TP_AMBIENTE = :_TP_AMBIENTE
              AND O.MANDT       = S.MANDT
              AND O.TP_AMBIENTE = S.TP_AMBIENTE
              AND S.CD_RECURSO  = :lwa_data_inbound-cd_recurso
              AND O.MANDT       = U.MANDT
              AND O.TP_AMBIENTE = U.TP_AMBIENTE
              AND U.CD_SISTEMA  = :lwa_data_inbound-cd_sistema


        ENDEXEC.
      CATCH cx_sy_native_sql_error INTO DATA(exc_ref).
        e_msg_erro = exc_ref->get_text( ).

        e_sucesso      = abap_true.
        e_nm_code      = '400'.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.
        RETURN.
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT REGISTROS INTO
        :LWA_DATA_RETORNO-URI,
        :LWA_DATA_RETORNO-AUTENTICACAO.

      ENDEXEC.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE REGISTROS
    ENDEXEC.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_retorno ).

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
