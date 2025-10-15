CLASS zcl_default_documents_step_01 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .

    DATA at_service TYPE /ui2/service_name .
    CLASS-DATA at_object TYPE REF TO zcl_default_documents_step_01 .
    DATA at_bapiret2_tab TYPE bapiret2_t .
    DATA at_auth_ws TYPE zauth_webservice .
    DATA at_system_id TYPE zin_id_processo .
    DATA at_id_processo TYPE string .

    METHODS execute_multipart
      IMPORTING
        !i_multi_tab        TYPE zde_multipart_field_t
        !i_params           TYPE string DEFAULT 'POST'
        !i_method           TYPE zde_http_metodo OPTIONAL
        !i_formato_body     TYPE zde_formato_body DEFAULT 'XML'
      RETURNING
        VALUE(r_integracao) TYPE zintegracao
      RAISING
        zcx_integracao
        zcx_error .
    METHODS execute_service
      IMPORTING
        !i_params           TYPE string
        !i_method           TYPE zde_http_metodo DEFAULT 'GET'
        !i_body             TYPE string OPTIONAL
        !i_formato_body     TYPE zde_formato_body DEFAULT 'XML'
      RETURNING
        VALUE(r_integracao) TYPE zintegracao
      RAISING
        zcx_integracao
        zcx_error .
    METHODS set_message
      IMPORTING
        !i_message TYPE string
        !i_msgty   TYPE msgty OPTIONAL
        !i_doc_id  TYPE string OPTIONAL .
    METHODS get_messages
      RETURNING
        VALUE(r_ret) TYPE bapiret2_t .
    METHODS process_contracts
      IMPORTING
        !ir_id_ref_range TYPE zinc_ref_range
        !iv_params       TYPE string OPTIONAL
      RETURNING
        VALUE(r_ret)     TYPE REF TO zcl_default_documents_step_01
      RAISING
        zcx_integracao
        zcx_error .
    METHODS set_system .
    METHODS get_documents
      IMPORTING
        !ir_id_ref_range TYPE zinc_ref_range OPTIONAL
        !iv_params       TYPE string OPTIONAL
      RAISING
        zcx_integracao
        zcx_error .
    METHODS before_sending
      IMPORTING
        !i_reference         TYPE string
      RETURNING
        VALUE(r_attachments) TYPE zins_adigital_attachments
      RAISING
        zcx_integracao
        zcx_error .
    METHODS send_to_approval
      RETURNING
        VALUE(r_ret) TYPE REF TO zif_integracao_adigital_get
      RAISING
        zcx_integracao
        zcx_error .
    METHODS after_sending
      IMPORTING
        !i_adigital  TYPE zint_assina01
      RETURNING
        VALUE(r_ret) TYPE REF TO zif_integracao_adigital_get
      RAISING
        zcx_integracao
        zcx_error .
    METHODS log_update
      RAISING
        zcx_integracao
        zcx_error .
    METHODS log_merge
      CHANGING
        !c_log_tab TYPE zinc_assina02 .
    METHODS log_api_send
      IMPORTING
        !i_log_tab TYPE zinc_assina02 .
    METHODS send_process_log
      IMPORTING
        !is_document TYPE zins_adigital_contract
        !it_anexos   TYPE zins_adigital_attachments .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DEFAULT_DOCUMENTS_STEP_01 IMPLEMENTATION.


  METHOD after_sending.
  ENDMETHOD.


  METHOD before_sending.
  ENDMETHOD.


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


  METHOD execute_service.

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


  METHOD get_documents.

  ENDMETHOD.


  METHOD get_messages.

    r_ret = me->at_bapiret2_tab.

  ENDMETHOD.


  METHOD log_api_send.
  ENDMETHOD.


  METHOD log_merge.

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


  METHOD log_update.

    DATA lv_cont TYPE i.
    DATA lt_log TYPE TABLE OF zint_assina02.

    DATA(lt_message) = me->get_messages( ).

    DATA(lv_inidt) = sy-datum.
    DATA(lv_fimdt) = sy-datum.

    SORT lt_message BY parameter ASCENDING.

    LOOP AT lt_message INTO DATA(lw_mess).

      READ TABLE lt_message ASSIGNING FIELD-SYMBOL(<fs_mess>) INDEX sy-tabix.

      CHECK sy-subrc EQ 0.

*      AT NEW parameter.
*        CLEAR lv_cont.
*      ENDAT.

      ADD 1 TO lv_cont.

      APPEND INITIAL LINE TO lt_log ASSIGNING FIELD-SYMBOL(<fs_log>).

      <fs_log>-id_referencia = COND #( WHEN me->at_id_processo IS INITIAL THEN <fs_mess>-parameter  "*-CS2021000218-10.11.2022-JT-#92371
                                                                          ELSE me->at_id_processo ).
      <fs_log>-id_processo   = at_system_id.
      <fs_log>-msgdt         = sy-datum.
      <fs_log>-msghr         = sy-uzeit.
      <fs_log>-id_mensagem   = lv_cont.
      <fs_log>-msgty         = <fs_mess>-type.
      <fs_log>-msgid         = <fs_mess>-number.
      <fs_log>-message       = <fs_mess>-message.
      <fs_log>-msgv1         = <fs_mess>-message_v1.
      <fs_log>-msgv2         = <fs_mess>-message_v2.
      <fs_log>-msgv3         = <fs_mess>-message_v3.
      <fs_log>-msgv4         = <fs_mess>-message_v4.

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

    TRY .


        " define o sistema
        me->set_system( ).

        " Recupera contratos
        me->get_documents( EXPORTING ir_id_ref_range = ir_id_ref_range iv_params = iv_params  ).

        " envia para aprovação
        me->send_to_approval( ).

        " atualiza log
        me->log_update( ).
      CATCH zcx_error.

    ENDTRY.

  ENDMETHOD.


  METHOD send_process_log.
  ENDMETHOD.


  METHOD send_to_approval.
  ENDMETHOD.


  METHOD set_message.

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
    DATA lv_len  TYPE  i VALUE 40. " 21.11.2024 - US #154395

    lv_texto = zcl_string2=>remove_spec_char( i_message ).

    REPLACE ALL OCCURRENCES OF '&' IN lv_texto WITH space.

    CONDENSE lv_texto.

    TRY .

        " 21.11.2024 - US #154395 -->
        IF strlen( lv_texto ) = 40.
          ADD 1 TO lv_len.
        ENDIF.
        " 21.11.2024 - US #154395 <--

        CALL FUNCTION 'TR_SPLIT_TEXT'
          EXPORTING
            iv_text  = lv_texto
            iv_len   = lv_len " 21.11.2024 - US #154395
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

    at_system_id = 'XX'. "definida no dominio 'ZIN_ID_PROCESSO'

  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

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

    e_sucesso = abap_true.

  ENDMETHOD.
ENDCLASS.
