class ZCL_INT_TOKEN_DESENV definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_TOKEN_DESENV .

  types:
    BEGIN OF ty_token,
      login      TYPE char20,
      password   TYPE string,
      tenantcode TYPE char10,
      clientid   TYPE char10,
    END OF ty_token .
  types:
    tyt_token TYPE STANDARD TABLE OF ty_token .

  data GT_DADOS type TYT_TOKEN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_TOKEN_DESENV IMPLEMENTATION.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso              = abap_true.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.

    r_if_integracao_inject = me.
    e_sucesso              = abap_true.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso              = abap_true.

  endmethod.


  METHOD zif_int_token_desenv~enviar_desenvolve.

    DATA lv_text2 TYPE char100.
    DATA lo_xml_ret TYPE REF TO cl_xml_document.

    CHECK it_saida[] IS NOT INITIAL.
    gt_dados[] = it_saida[].

    me->zif_int_token_desenv~set_servico( i_servico = me->zif_int_token_desenv~gc_token ).
    me->zif_int_token_desenv~set_id_referencia( ).
    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_desenv_token.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
*    me->zif_int_token_desenv~at_struct              = it_saida.

*    SELECT SINGLE *
*      FROM zauth_webservice
*      INTO me->zif_int_user_desenv~at_auth_ws
*        WHERE service = me->zif_int_user_desenv~gc_service.

    SELECT SINGLE *
      FROM zauth_webservice
      INTO me->zif_int_token_desenv~at_token_ws
        WHERE service = me->zif_int_token_desenv~gc_token.

    IF me->zif_int_token_desenv~at_token_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

    CREATE OBJECT lo_xml_ret.

    CLEAR r_return_value.

*    me->zif_int_token_desenv~set_ds_url(
*      )->set_ds_data(
*      )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao  = DATA(e_integracao) ).
*
*    DATA: ls_struct TYPE ty_struct.

*    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = ls_struct ).

*    IF ls_struct-importeditems LE 0.
*      ev_return_code = ls_struct-importeditems.
*      ev_return_msg = 'ERROR'.
*      SHIFT ev_return_msg LEFT DELETING LEADING space.
*      zcx_error=>zif_error~gera_erro_geral( i_texto = ev_return_msg ).
*    ELSE.
*      ev_return_code = ls_struct-importeditems.
*      FREE: ev_return_msg.
*    ENDIF.

  ENDMETHOD.


  METHOD zif_int_token_desenv~get_id_referencia.

    r_if_int_token_desenv = me.
    e_referencia-tp_referencia = me->zif_int_token_desenv~gc_token.

    READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>) INDEX 1.
    IF sy-subrc IS INITIAL AND <fs_dados> IS ASSIGNED AND <fs_dados>-login IS NOT INITIAL.
      e_referencia-id_referencia = <fs_dados>-login.
    ELSE.
      e_referencia-id_referencia = sy-datum.
    ENDIF.

  ENDMETHOD.


  METHOD zif_int_token_desenv~get_instance.

    IF zif_int_token_desenv~at_if_integracao_token IS NOT BOUND.
      CREATE OBJECT zif_int_token_desenv~at_if_integracao_token
        TYPE zcl_int_token_desenv.
    ENDIF.

    r_if_integracao_token = zif_int_token_desenv~at_if_integracao_token.

  ENDMETHOD.


  METHOD zif_int_token_desenv~get_token.

    DATA: l_json              TYPE string,
          l_table_timestamp   TYPE p,
          ls_token            TYPE ty_token,
          lt_token            TYPE tyt_token,
          l_current_timestamp TYPE p.

    DATA: lt_zmmt0159 TYPE STANDARD TABLE OF zmmt0159,
          lt_tvarvc   TYPE STANDARD TABLE OF tvarvc.

    me->zif_int_token_desenv~set_servico( i_servico = me->zif_int_token_desenv~gc_token ).
    me->zif_int_token_desenv~set_id_referencia( ).
    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_desenv_token.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.

*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Ini - Altera a cada solicitação
*--------------------------------------------------------------------------------------------------------------------*
    SELECT *
      FROM tvarvc
      INTO TABLE lt_tvarvc
      WHERE name EQ 'DESENV_CHECK_TOKEN_VALIDO'
        AND low  EQ abap_true.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE MAX( time_stamp )
        FROM zmmt0159 INTO @DATA(lwa_max_tim_stamp).

      SELECT SINGLE *
        FROM zmmt0159 INTO @DATA(ls_zmmt0159)
        WHERE time_stamp = @lwa_max_tim_stamp.

      IF sy-subrc IS INITIAL.

        l_table_timestamp   = ls_zmmt0159-time_stamp.

        GET TIME STAMP FIELD DATA(l_timestamp_before).
        l_current_timestamp = l_timestamp_before.

        IF cl_abap_tstmp=>subtract( tstmp1 = l_current_timestamp
                                    tstmp2 = l_table_timestamp ) < 80000. "+/-22h?

          "Token ainda está válido.
          CONCATENATE 'Bearer' ls_zmmt0159-access_token INTO DATA(l_header_token) SEPARATED BY space.
          CLEAR: me->zif_integracao_inject~at_header_fields.
          APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.
          APPEND VALUE #( name = 'Ocp-Apim-Subscription-Key'  value = 'bae9a95f684a420685bc3977a70deebb' ) TO me->zif_integracao_inject~at_header_fields.
          EXIT.
        ENDIF.

      ENDIF.

    ENDIF.
*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Fim
*--------------------------------------------------------------------------------------------------------------------*
    FREE: l_json.

    SELECT *
      FROM zauth_webservice
      INTO TABLE @DATA(lt_werbservice)
      WHERE service EQ 'DESENV_INT_TOKEN'.
    IF sy-subrc IS INITIAL.
      READ TABLE lt_werbservice INTO DATA(ls_webservice) INDEX 1.

      ls_token-login = ls_webservice-username.
      ls_token-password = ls_webservice-password.
      ls_token-clientid = ls_webservice-add01.
      TRANSLATE ls_webservice-add02 TO LOWER CASE.
      ls_token-tenantcode = ls_webservice-add02.
      APPEND ls_token TO lt_token.
    ENDIF.

*    l_json = /ui2/cl_json=>serialize( data = ls_token ).
*    me->zif_integracao_inject~at_info_request_http-ds_body = l_json.
*    me->zif_int_token_desenv~at_body = me->zif_integracao_inject~at_info_request_http-ds_body.
    me->zif_int_token_desenv~at_struct              = ls_token.

    APPEND VALUE #( name = 'Ocp-Apim-Subscription-Key'  value = 'bae9a95f684a420685bc3977a70deebb' ) TO me->zif_integracao_inject~at_header_fields.
    r_if_integracao_token = me.
    me->zif_int_token_desenv~set_ds_url( ).
    me->zif_int_token_desenv~set_ds_data( ).
    me->zif_int_token_desenv~set_send_msg( IMPORTING
                                                  e_access_token  = DATA(e_access_token)
                                                  e_token_type    = DATA(e_token_type)
                                                  e_expires_in    = DATA(e_expires_in)
                                          ).

    GET TIME STAMP FIELD DATA(l_timestamp_after).

    APPEND VALUE #( access_token = e_access_token token_type = e_token_type expires_in = e_expires_in time_stamp = l_timestamp_after ) TO lt_zmmt0159.

    MODIFY zmmt0159 FROM TABLE lt_zmmt0159.

    r_if_integracao_token = me.
  ENDMETHOD.


  method ZIF_INT_TOKEN_DESENV~SET_DS_DATA.

     me->zif_integracao_inject~at_info_request_http-ds_body = /ui2/cl_json=>serialize( data = me->zif_int_token_desenv~at_struct ).
     me->zif_int_token_desenv~at_body = me->zif_integracao_inject~at_info_request_http-ds_body.
     r_if_int_token_desenv = me.

  endmethod.


  METHOD zif_int_token_desenv~set_ds_url.

    DATA: lt_werbservice TYPE STANDARD TABLE OF zauth_webservice.

    DATA: wa_webservice TYPE zauth_webservice.



    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_int_token_desenv~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_int_token_desenv~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_int_token_desenv~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_int_token_desenv~set_id_referencia( ).
*    r_if_int_user_desenv = me.



    r_if_integracao_token = me.

    SELECT *
      FROM zauth_webservice
      INTO TABLE lt_werbservice
      WHERE service EQ 'DESENV_INT_TOKEN'.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'C'
                            attr2 = 'C1' )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'C'
          msgv2  = 'C1'.
    ENDIF.

    SORT lt_werbservice BY service.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Ocp-Apim-Subscription-Key' value = 'bae9a95f684a420685bc3977a70deebb' ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/json'.

    READ TABLE lt_werbservice INTO wa_webservice WITH KEY service = 'DESENV_INT_TOKEN' BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = wa_webservice-url.
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.


  ENDMETHOD.


  method ZIF_INT_TOKEN_DESENV~SET_ID_REFERENCIA.

    r_if_int_token_desenv = me.
    me->zif_int_token_desenv~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  METHOD zif_int_token_desenv~set_send_msg.

    TYPES: BEGIN OF ty_retorno,
             authenticationtoken TYPE string,
           END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    r_if_integracao_token = me.

** ------------------
*
*    DATA(obj_webservice) = NEW zcl_webservice( ).
*    DATA: lv_string TYPE string.
*    DATA: lv_name TYPE string,
*          i_json  TYPE string,
*          r_json  TYPE string.
*
*    DATA ls_url TYPE string.
*
*    ls_url = me->zif_integracao_inject~at_info_request_http-ds_url.
*
*    cl_http_client=>create_by_url(
*         EXPORTING
*           url                = ls_url
*           "'https://desenvolveapim.azure-api.net/integrationtest/v1/.auth/login/custom'
*         IMPORTING
*           client             = DATA(e_http)
*         EXCEPTIONS
*           argument_not_found = 1
*           plugin_not_active  = 2
*           internal_error     = 3
*           OTHERS             = 4 ).
*
*    CALL METHOD e_http->request->set_header_field
*      EXPORTING
*        name  = '~request_method'
*        value = 'POST'.
*
*    CALL METHOD e_http->request->set_header_field
*      EXPORTING
*        name  = 'Content-Type'
*        value = 'application/json'.
*
*    CALL METHOD e_http->request->set_header_field
*      EXPORTING
*        name  = 'Ocp-Apim-Subscription-Key'
*        value = 'bae9a95f684a420685bc3977a70deebb'.
*
*    i_json = me->zif_integracao_inject~at_info_request_http-ds_body.
*
*    obj_webservice->zif_webservice~consultar(
*      EXPORTING
*        i_http                     = e_http
*        i_xml                      = i_json
*        i_not_content_length       = abap_true
*      IMPORTING
*        e_reason                   = DATA(e_reason)
*      RECEIVING
*        e_resultado                = r_json
*      EXCEPTIONS
*        http_communication_failure = 1
*        http_invalid_state         = 2
*        http_processing_failed     = 3
*        http_invalid_timeout       = 4
*        OTHERS                     = 5 ).
*
**-------------------------

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    FREE: lc_integrar.

    IF e_integracao-ds_data_retorno IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).
    ENDIF.

*    IF e_integracao-ds_data_retorno IS INITIAL AND r_json IS NOT INITIAL.
*      /ui2/cl_json=>deserialize( EXPORTING json = r_json CHANGING data = lc_retorno ).
*
*    ELSE.
*      /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).
*    ENDIF.

    e_access_token = lc_retorno-authenticationtoken.
    e_token_type   = 'Bearer'.

    CONCATENATE e_token_type e_access_token INTO DATA(l_header_token) SEPARATED BY space.
    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Ocp-Apim-Subscription-Key'  value = 'bae9a95f684a420685bc3977a70deebb' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_int_token_desenv~set_servico.

    IF i_servico IS NOT INITIAL.
      zif_int_token_desenv~at_servico = i_servico.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
