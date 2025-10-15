CLASS zcl_int_ib_cons_param_fardos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_group_options,
        werks_key TYPE flag,
      END OF ty_group_options .

    DATA:
*
      BEGIN OF zde_data_request,
        charg            TYPE TABLE OF ztsafrafardos-charg,
        werks_from       TYPE TABLE OF ztsafrafardos-werks_from,
        werks_to         TYPE TABLE OF ztsafrafardos-werks_to,
        werks_key        TYPE TABLE OF ztsafrafardos-werks_key,
        grouping_options TYPE ty_group_options,
      END OF zde_data_request .


    DATA:
      BEGIN OF zde_data_response,
        parametros TYPE TABLE OF ztsafrafardos,
      END OF zde_data_response.

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '140' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_PARAM_FARDOS IMPLEMENTATION.


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
    IF lwa_data_request-charg IS INITIAL AND lwa_data_request-werks_from IS INITIAL AND
       lwa_data_request-werks_key IS INITIAL AND lwa_data_request-werks_to IS INITIAL.
      r_msg_erro = 'Necessário informar algum filtro!'.
      RETURN.
    ENDIF.


  ENDMETHOD.


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

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response  LIKE zde_data_response.


    DATA: lra_useralias  TYPE RANGE OF usalias,
          lra_monat      TYPE RANGE OF monat,
          lra_gjahr      TYPE RANGE OF gjahr,
          lra_charg      TYPE RANGE OF ztsafrafardos-charg,
          lra_werks_from TYPE RANGE OF ztsafrafardos-werks_from,
          lra_werks_to   TYPE RANGE OF ztsafrafardos-werks_to,
          lra_werks_key  TYPE RANGE OF ztsafrafardos-werks_key.

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

    LOOP AT lwa_data_request-charg ASSIGNING FIELD-SYMBOL(<fs_charg>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_charg> ) TO lra_charg.
    ENDLOOP.

    LOOP AT lwa_data_request-werks_from ASSIGNING FIELD-SYMBOL(<fs_werks_from>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_werks_from> ) TO lra_werks_from.
    ENDLOOP.

    LOOP AT lwa_data_request-werks_to ASSIGNING FIELD-SYMBOL(<fs_werks_to>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_werks_to> ) TO lra_werks_to.
    ENDLOOP.

    LOOP AT lwa_data_request-werks_key ASSIGNING FIELD-SYMBOL(<fs_werks_key>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_werks_key> ) TO lra_werks_key.
    ENDLOOP.

    IF lwa_data_request-grouping_options-werks_key IS NOT INITIAL.

      SELECT werks_key
        FROM ztsafrafardos
        INTO CORRESPONDING FIELDS OF TABLE lwa_data_response-parametros
       WHERE charg      IN lra_charg
         AND werks_from IN lra_werks_from
         AND werks_to   IN lra_werks_to
         AND werks_key  IN lra_werks_key
    GROUP BY werks_key.

    ELSE.

      SELECT *
        FROM ztsafrafardos
        INTO CORRESPONDING FIELDS OF TABLE lwa_data_response-parametros
       WHERE charg      IN lra_charg
         AND werks_from IN lra_werks_from
         AND werks_to   IN lra_werks_to
         AND werks_key  IN lra_werks_key.

    ENDIF.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

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
