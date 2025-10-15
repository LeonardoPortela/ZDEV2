CLASS zcl_int_sigam_hedge_reverter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_int_sigam_reverter_hedge .

    METHODS constructor
      RAISING
        zcx_integracao
        zcx_error .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS converte_response_itab
      IMPORTING
        !i_response_json TYPE string
      RETURNING
        VALUE(r_ret_tab) TYPE zsdc0028 .
ENDCLASS.



CLASS ZCL_INT_SIGAM_HEDGE_REVERTER IMPLEMENTATION.


  METHOD constructor.

    me->zif_int_sigam_reverter_hedge~at_servico      = 'SIGAM_REVERTER_HEDGE'.
    me->zif_integracao_inject~at_id_interface    = '097'. " zif_integracao=>at_id_interf_sigam_hedge_rever. " Quando a request do AGRO PM subir esse codigo pode ser descomentado
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    "me->zif_integracao_inject~at_autentica_api_ad = zif_integracao=>at_id_interface_aut_api_ad_sim.
    "me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    "me->zif_integracao_inject~at_autentica_module = 'goflux'.

    me->zif_integracao_inject~at_autentica_api_ad = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.
    me->zif_integracao_inject~at_autentica_module = ''.

    IF me->zif_int_sigam_reverter_hedge~at_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_int_sigam_reverter_hedge~at_auth_ws
          WHERE service = me->zif_int_sigam_reverter_hedge~at_servico .

    ENDIF.

    IF me->zif_int_sigam_reverter_hedge~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD converte_response_itab.

    DATA lt_prod_perm TYPE TABLE OF cvp_s_lifnr.

    DATA lw_req TYPE zsde0027.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_response_json
      CHANGING
        data = lw_req.

    LOOP AT lw_req-data ASSIGNING FIELD-SYMBOL(<fs_struc>).

      APPEND INITIAL LINE TO r_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

      <fs_ret>-identificaoreversa = <fs_struc>-identificaoreversa.

    ENDLOOP.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    "APPEND VALUE #( name = 'Content-Type' value = 'application/json' ) TO me->zif_integracao_inject~at_header_fields.

    r_if_integracao_inject = me.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


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

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_int_sigam_reverter_hedge~enviar_sigam.

    IF i_request IS INITIAL.
      zcx_error=>zif_error~gera_erro_geral( i_texto = 'Informar dados a serem revertidos' ).
    ENDIF.

    DATA(lv_request) = /ui2/cl_json=>serialize( data = i_request pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

    zif_int_sigam_reverter_hedge~get_instance(
      )->set_ds_url(
      )->set_ds_data( EXPORTING i_request = lv_request
      )->set_send_msg( IMPORTING e_integracao = DATA(lw_ret) ).


    IF lw_ret-nm_code < 205.
      MESSAGE s016(ds) WITH 'Dados consultados com sucesso'.
    ELSE.

      DATA ls_erro TYPE zsde0029.

      REPLACE ALL OCCURRENCES OF 'u00E3' IN lw_ret-ds_data_retorno WITH 'ã'.
      REPLACE ALL OCCURRENCES OF 'u00E9' IN lw_ret-ds_data_retorno WITH 'é'.

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json        = lw_ret-ds_data_retorno
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
          data        = ls_erro.

      sy-msgv1 = ls_erro-message(40).
      sy-msgv2 = ls_erro-message+40(40).
      sy-msgv3 =  ls_erro-message+80(40).
      sy-msgv4 = ls_erro-message+120(40).

      MESSAGE s000(shdb) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

    ENDIF.

    r_respo_tab = converte_response_itab( i_response_json = lw_ret-ds_data_retorno ).

  ENDMETHOD.


  METHOD zif_int_sigam_reverter_hedge~get_instance.

    IF zif_int_sigam_reverter_hedge~at_instance IS NOT BOUND.

      zif_int_sigam_reverter_hedge~at_instance = NEW zcl_int_sigam_hedge_reverter( ).

    ENDIF.

    r_object = zif_int_sigam_reverter_hedge~at_instance.


  ENDMETHOD.


  METHOD zif_int_sigam_reverter_hedge~set_ds_data.

    me->zif_int_sigam_reverter_hedge~at_body = i_request. "/ui2/cl_json=>serialize( data = i_request ).

    r_object = me.

  ENDMETHOD.


  METHOD zif_int_sigam_reverter_hedge~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    "me->zif_integracao_inject~at_info_request_http-ds_form_data = me->ZIF_INT_SIGAM_REVERTER_HEDGE~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_int_sigam_reverter_hedge~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_int_sigam_reverter_hedge~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_int_sigam_reverter_hedge~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_int_sigam_reverter_hedge~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = me->zif_int_sigam_reverter_hedge~at_auth_ws-url_token.

    r_object = me.

  ENDMETHOD.


  METHOD zif_int_sigam_reverter_hedge~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_object = me.

    CREATE OBJECT lc_integrar.

*    lc_integrar->zif_integracao~at_msg_integra-ds_url_token = me->ZIF_INT_SIGAM_REVERTER_HEDGE~at_auth_ws-url_token.
*    lc_integrar->zif_integracao~at_msg_integra-ds_body = me->ZIF_INT_SIGAM_REVERTER_HEDGE~at_body.
*
    me->zif_integracao_inject~at_info_request_http-ds_body = me->zif_int_sigam_reverter_hedge~at_body.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = me->zif_int_sigam_reverter_hedge~at_auth_ws-url_token.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

*    IF e_integracao-nm_code < 205.
*      MESSAGE s016(ds) WITH e_integracao-ds_data_retorno.
*    ELSE.
*      MESSAGE s016(ds) WITH e_integracao-ds_data_retorno DISPLAY LIKE 'E'.
*    ENDIF.


    CLEAR: lc_integrar.

    r_object = me.



  ENDMETHOD.
ENDCLASS.
