class ZCL_DEFAULT_DOCUMENTS_STEP_02 definition
  public
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .

  data AT_SERVICE type /UI2/SERVICE_NAME .
  class-data AT_OBJECT type ref to ZCL_DEFAULT_DOCUMENTS_STEP_02 .
  data AT_BAPIRET2_TAB type BAPIRET2_T .
  data AT_AUTH_WS type ZAUTH_WEBSERVICE .
  data AT_SYSTEM_ID type ZIN_ID_PROCESSO .

  methods EXECUTE_MULTIPART
    importing
      !I_MULTI_TAB type ZDE_MULTIPART_FIELD_T
      !I_PARAMS type STRING default 'POST'
      !I_METHOD type ZDE_HTTP_METODO optional
      !I_FORMATO_BODY type ZDE_FORMATO_BODY default 'XML'
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods EXECUTE_SERVICE
    importing
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'GET'
      !I_BODY type STRING optional
      !I_FORMATO_BODY type ZDE_FORMATO_BODY default 'XML'
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods SET_SYSTEM .
  methods SET_MESSAGE
    importing
      !I_MESSAGE type STRING
      !I_MSGTY type MSGTY optional
      !I_DOC_ID type STRING optional .
  methods GET_MESSAGES
    returning
      value(R_RET) type BAPIRET2_T .
  methods PROCESS_CONTRACTS
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods LOG_UPDATE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  methods LOG_MERGE
    changing
      !C_LOG_TAB type ZINC_ASSINA02 .
  methods LOG_API_SEND
    importing
      !I_LOG_TAB type ZINC_ASSINA02 .
  methods PUBLISH_CONTRACT
    importing
      !IR_ID_REF_RANGE type ZINC_REF_RANGE
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEFAULT_DOCUMENTS_STEP_02 IMPLEMENTATION.


  METHOD execute_multipart.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->at_service IS NOT INITIAL.

      IF me->at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->at_auth_ws
            WHERE service = me->at_service.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF me->at_auth_ws  IS INITIAL.
      lv_msgtx = `Serviço ` && me->at_service && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    REPLACE '#PARAMS#' IN me->at_auth_ws-url WITH space.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = i_formato_body.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'multipart/form-data'.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->at_auth_ws-url && i_params.
    "me->zif_integracao_inject~at_multipart_fields = i_multi_tab.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    zcl_string2=>gui_indicator_url( me->zif_integracao_inject~at_info_request_http-ds_url ) .

    CREATE OBJECT lc_integrar.


    lc_integrar->zif_integracao~at_multipart = i_multi_tab.
    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = r_integracao
      )->free(
      ).

    FREE lc_integrar.

  ENDMETHOD.


  METHOD EXECUTE_SERVICE.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF me->at_service IS NOT INITIAL.

      IF me->at_auth_ws IS INITIAL.

        SELECT SINGLE * FROM zauth_webservice
          INTO me->at_auth_ws
            WHERE service = me->at_service.

      ENDIF.

    ELSE.

      lv_msgtx = 'O nome do serviço não pode ser vazio'.

      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).

    ENDIF.

    IF me->at_auth_ws  IS INITIAL.
      lv_msgtx = `Serviço ` && me->at_service && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = i_formato_body.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->at_auth_ws-url && i_params.
    CLEAR me->zif_integracao_inject~at_multipart_fields.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    CREATE OBJECT lc_integrar.

    IF i_body IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_body = i_body.
    ELSE.
      CLEAR me->zif_integracao_inject~at_info_request_http-ds_body.
    ENDIF.

    zcl_string2=>gui_indicator_url( me->zif_integracao_inject~at_info_request_http-ds_url ).

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = r_integracao
      )->free(
      ).

    FREE lc_integrar.

  ENDMETHOD.


  METHOD GET_MESSAGES.

    r_ret = me->at_bapiret2_tab.

  ENDMETHOD.


METHOD log_api_send.

ENDMETHOD.


METHOD LOG_MERGE.

  " se a tabela de log nao estiver vazia
  CHECK c_log_tab IS NOT INITIAL.

  " ve se encontra mensagens já salvas
  SELECT * FROM zint_assina02
    INTO TABLE @DATA(lt_log)
      FOR ALL ENTRIES IN @c_log_tab
        WHERE id_referencia = @c_log_tab-id_referencia
          AND id_processo = @c_log_tab-id_processo
          AND id_mensagem = @c_log_tab-id_mensagem
          AND msgty = @c_log_tab-msgty
          AND msgid = @c_log_tab-msgid.

  CHECK sy-subrc EQ 0.

  " se encontrar elimina do log atual, não precisa gravar duas vezes
  LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).

    DELETE c_log_tab WHERE id_referencia = <fs_log>-id_referencia
                       AND id_processo = <fs_log>-id_processo
                       AND id_mensagem = <fs_log>-id_mensagem
                       AND msgty = <fs_log>-msgty
                       AND msgid = <fs_log>-msgid.

    CHECK c_log_tab IS INITIAL.

    EXIT.

  ENDLOOP.


ENDMETHOD.


  METHOD LOG_UPDATE.

    DATA lv_cont TYPE i.
    DATA lt_log TYPE TABLE OF zint_assina02.

    DATA(lt_message) = me->get_messages( ).

    DATA(lv_inidt) = sy-datum.
    DATA(lv_fimdt) = sy-datum.

    SORT lt_message BY parameter ASCENDING.

    LOOP AT lt_message INTO DATA(lw_mess).

      READ TABLE lt_message ASSIGNING FIELD-SYMBOL(<fs_mess>) INDEX sy-tabix.

      CHECK sy-subrc EQ 0.

      AT NEW parameter.
        CLEAR lv_cont.
      ENDAT.

      ADD 1 TO lv_cont.

      APPEND INITIAL LINE TO lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).

      <fs_log>-id_referencia = <fs_mess>-parameter.
      <fs_log>-id_processo = at_system_id.
      <fs_log>-msgdt = sy-datum.
      <fs_log>-msghr = sy-uzeit.
      <fs_log>-id_mensagem = lv_cont.
      <fs_log>-msgty = <fs_mess>-type.
      <fs_log>-msgid = <fs_mess>-number.
      <fs_log>-message = <fs_mess>-message.
      <fs_log>-msgv1 = <fs_mess>-message_v1.
      <fs_log>-msgv2 = <fs_mess>-message_v2.
      <fs_log>-msgv3 = <fs_mess>-message_v3.
      <fs_log>-msgv4 = <fs_mess>-message_v4.

    ENDLOOP.

    log_merge( CHANGING c_log_tab = lt_log ).

    MODIFY zint_assina02 FROM TABLE lt_log.

    CHECK lt_log IS NOT INITIAL.

    log_api_send( lt_log ).

    SUBTRACT 40 FROM lv_inidt.
    SUBTRACT 10 FROM lv_fimdt.

    DELETE FROM zint_assina02 WHERE msgdt BETWEEN lv_inidt AND lv_fimdt.

  ENDMETHOD.


  METHOD process_contracts.

    " define o sistema
    me->set_system( ).

    " Recupera contratos
    me->publish_contract( ir_id_ref_range ).

    " atualiza log
    me->log_update( ).

  ENDMETHOD.


  method PUBLISH_CONTRACT.
  endmethod.


  METHOD SET_MESSAGE.

    DATA(lv_msgty) = i_msgty.

    CHECK i_message IS NOT INITIAL.

    IF lv_msgty IS INITIAL.
      lv_msgty = 'E'.
    ENDIF.

    APPEND INITIAL LINE TO me->at_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_message>).

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    DATA lv_msg1 TYPE sy-msgv1.
    DATA lv_msg2 TYPE sy-msgv1.
    DATA lv_msg3 TYPE sy-msgv1.
    DATA lv_msg4 TYPE sy-msgv1.

    lv_texto = zcl_string2=>remove_spec_char( i_message ).

    REPLACE ALL OCCURRENCES OF '&' IN lv_texto WITH space.

    CONDENSE lv_texto.

    TRY .

        CALL FUNCTION 'TR_SPLIT_TEXT'
          EXPORTING
            iv_text  = lv_texto
            iv_len   = 40
          IMPORTING
            et_lines = lt_trtexts.

    ENDTRY.


    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

      CASE sy-tabix.
        WHEN 1.
          <fs_message>-message_v1 = <fs_line>.
        WHEN 2.
          <fs_message>-message_v2 = <fs_line>.
        WHEN 3.
          <fs_message>-message_v3 = <fs_line>.
        WHEN 4.
          <fs_message>-message_v4 = <fs_line>.
      ENDCASE.

    ENDLOOP.

    <fs_message>-id = 'DS'.
    <fs_message>-type = lv_msgty.
    <fs_message>-number = '016'.
    <fs_message>-parameter = i_doc_id.

    MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number
      WITH <fs_message>-message_v1 <fs_message>-message_v2
           <fs_message>-message_v3 <fs_message>-message_v4 INTO <fs_message>-message.


  ENDMETHOD.


METHOD set_system.

  at_system_id = 'XX'.

ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.

    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
