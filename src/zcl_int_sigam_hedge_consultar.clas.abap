class ZCL_INT_SIGAM_HEDGE_CONSULTAR definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_SIGAM_CONSULTAR_HEDGE .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO
      ZCX_ERROR .
  PROTECTED SECTION.
private section.

  class-methods TO_QUERY
    importing
      !I_REQUEST type ZSDE0020
    returning
      value(RV_QUERY) type STRING .
  class-methods CONVERTE_RESPONSE_ITAB
    importing
      !I_RESPONSE_JSON type STRING
    returning
      value(R_RET_TAB) type ZSDC0025 .
ENDCLASS.



CLASS ZCL_INT_SIGAM_HEDGE_CONSULTAR IMPLEMENTATION.


  METHOD constructor.

    me->zif_int_sigam_consultar_hedge~at_servico      = 'SIGAM_CONSULTAR_HEDGE'.
    me->zif_integracao_inject~at_id_interface    = '096'. "zif_integracao=>at_id_interf_sigam_hedge_consu. " quando subir a request do Agro PM, esse codigo pode ser descomentado
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

    IF me->zif_int_sigam_consultar_hedge~at_servico IS NOT INITIAL.

      SELECT SINGLE * FROM zauth_webservice
        INTO me->zif_int_sigam_consultar_hedge~at_auth_ws
          WHERE service = me->zif_int_sigam_consultar_hedge~at_servico .

    ENDIF.

    IF me->zif_int_sigam_consultar_hedge~at_auth_ws  IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

  ENDMETHOD.


  METHOD converte_response_itab.

    DATA lt_prod_perm TYPE TABLE OF cvp_s_lifnr.

    DATA lw_req TYPE zsdc0024.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = i_response_json
      CHANGING
        data = lw_req.

    LOOP AT lw_req-data ASSIGNING FIELD-SYMBOL(<fs_struc>).

      APPEND INITIAL LINE TO r_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

      <fs_ret>-identificacao = <fs_struc>-identificacao.
      <fs_ret>-sequenciaboleta = <fs_struc>-sequenciaboleta.
      <fs_ret>-idobrigacao = <fs_struc>-idobrigacao.
      <fs_ret>-tipoobrigacao = <fs_struc>-tipoobrigacao.

      REPLACE ALL OCCURRENCES OF '-' IN <fs_struc>-dtvencimento WITH ''.

      <fs_ret>-dtvencimento = <fs_struc>-dtvencimento.

      <fs_ret>-vrreal = <fs_struc>-vrreal.
      <fs_ret>-vrdolar = <fs_struc>-vrdolar.
      <fs_ret>-txdolarnegociado = <fs_struc>-txdolarnegociado.
      <fs_ret>-vrdolarnegociado = <fs_struc>-vrdolarnegociado.
      <fs_ret>-txdolarcurva = <fs_struc>-txdolarcurva.
      <fs_ret>-vrdolarcurva = <fs_struc>-vrdolarcurva.
      <fs_ret>-saldo = <fs_struc>-saldo.
      <fs_ret>-comentarios = <fs_struc>-comentarios.

    ENDLOOP.

  ENDMETHOD.


  METHOD to_query.

    DATA lv_lista TYPE c LENGTH 30.

    rv_query = ''.

    IF i_request IS NOT INITIAL.

      IF i_request-dtvencimento IS NOT INITIAL.
        rv_query = rv_query && '&DTVENCIMENTO=' && i_request-dtvencimento.
      ENDIF.

      IF i_request-codempresa IS NOT INITIAL.
        rv_query = rv_query && '&CODEMPRESA=' && i_request-codempresa.
      ENDIF.

      IF i_request-tpboletalista IS NOT INITIAL.

        rv_query = rv_query && '&TPBOLETALISTA='.

        CLEAR lv_lista.

        LOOP AT i_request-tpboletalista ASSIGNING FIELD-SYMBOL(<fs_list>).
          lv_lista = lv_lista && ',' && <fs_list>.
        ENDLOOP.

        lv_lista = lv_lista+1.

        rv_query = rv_query && lv_lista.

      ENDIF.


      IF i_request-tiposobrigacao IS NOT INITIAL.

        rv_query = rv_query && '&TIPOSOBRIGACAO='.

        CLEAR lv_lista.

        LOOP AT i_request-tiposobrigacao ASSIGNING <fs_list>.
          lv_lista = lv_lista && ',' && <fs_list>.
        ENDLOOP.

        lv_lista = lv_lista+1.

        rv_query = rv_query && lv_lista.

      ENDIF.

      rv_query = rv_query+1.

    ENDIF.

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


  METHOD zif_int_sigam_consultar_hedge~enviar_sigam.

    IF i_request IS INITIAL.
      zcx_error=>zif_error~gera_erro_geral( i_texto = 'Informar dados a serem consultados' ).
    ENDIF.

    DATA(lv_query) = to_query( i_request = i_request ).

    zif_int_sigam_consultar_hedge~get_instance(
      )->set_ds_url( EXPORTING iv_query = lv_query
      )->set_ds_data(
      )->set_send_msg( IMPORTING e_integracao = DATA(lw_ret) ).


    IF lw_ret-nm_code < 205.
      MESSAGE s016(ds) WITH 'Dados consultados com sucesso'.
    ELSE.
      MESSAGE s016(ds) WITH 'Erro ao reverter' DISPLAY LIKE 'E'.
    ENDIF.

    r_respo_tab = converte_response_itab( i_response_json = lw_ret-ds_data_retorno ).

  ENDMETHOD.


  METHOD zif_int_sigam_consultar_hedge~get_instance.

    IF zif_int_sigam_consultar_hedge~at_instance IS NOT BOUND.

      zif_int_sigam_consultar_hedge~at_instance = NEW zcl_int_sigam_hedge_consultar( ).

    ENDIF.

    r_object = zif_int_sigam_consultar_hedge~at_instance.


  ENDMETHOD.


  METHOD zif_int_sigam_consultar_hedge~set_ds_data.

    r_object = me.

  ENDMETHOD.


  METHOD zif_int_sigam_consultar_hedge~set_ds_url.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    "me->zif_integracao_inject~at_info_request_http-ds_form_data = me->ZIF_INT_SIGAM_CONSULTAR_HEDGE~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_int_sigam_consultar_hedge~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_int_sigam_consultar_hedge~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_int_sigam_consultar_hedge~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_int_sigam_consultar_hedge~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = me->zif_int_sigam_consultar_hedge~at_auth_ws-url_token.

    IF iv_query IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_inject~at_info_request_http-ds_url && '?' && iv_query.
    ENDIF.

    r_object = me.

  ENDMETHOD.


  METHOD zif_int_sigam_consultar_hedge~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_object = me.

    CREATE OBJECT lc_integrar.

*    lc_integrar->zif_integracao~at_msg_integra-ds_url_token = me->zif_int_sigam_consultar_hedge~at_auth_ws-url_token.
*    lc_integrar->zif_integracao~at_msg_integra-ds_body = me->zif_int_sigam_consultar_hedge~at_body.
*
    me->zif_integracao_inject~at_info_request_http-ds_body = me->zif_int_sigam_consultar_hedge~at_body.
    me->zif_integracao_inject~at_info_request_http-ds_url_token = me->zif_int_sigam_consultar_hedge~at_auth_ws-url_token.


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
