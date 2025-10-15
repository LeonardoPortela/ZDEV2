class ZCL_INT_OB_CONS_FARDO_TRACE_CT definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '213' ##NO_TEXT.
  data AT_DADOS_ENVIO type ZPPS0008 .
  data AT_DADOS_RETORNO type ZPPS0007_T .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  methods GET_FARDOS_RETORNADOS
    exporting
      value(E_FARDOS) type ZPPS0007_T
    returning
      value(R_IF_INTEGRACAO_OUTBOUND) type ref to ZIF_INTEGRACAO_OUTBOUND .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_INT_OB_CONS_FARDO_TRACE_CT IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.


  ENDMETHOD.


  method GET_FARDOS_RETORNADOS.

    r_if_integracao_outbound = me.

    CLEAR: e_fardos[].

    e_fardos = ME->at_dados_retorno[].


  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    r_if_integracao_inject = me.

    CHECK me->zif_integracao_inject~at_header_fields IS INITIAL.

    TRY.

        CAST zcl_int_ob_token_trace_cotton(
               zcl_int_ob_token_trace_cotton=>zif_integracao_outbound~get_instance(
                 )->execute_request( )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST.

    r_if_integracao_outbound = me.

    MOVE-CORRESPONDING i_info_request to me->at_dados_envio.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~EXECUTE_REQUEST.

    DATA: lva_msg TYPE c LENGTH 50.
    DATA: lva_msg_retorno TYPE string.
    DATA: lit_zmmt0008 TYPE TABLE OF zmmt0008.

    r_if_integracao_outbound = me.

    CLEAR: ME->at_dados_retorno[].

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_outbound~build_info_request( i_info_request = i_info_request
      )->get_data( IMPORTING e_data = DATA(lc_data)
      )->set_data( EXPORTING i_data = lc_data
      )->set_url(
      )->set_id_referencia(
      )->send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

    CHECK e_integracao-ds_data_retorno IS NOT INITIAL.

    REPLACE ALL OCCURRENCES OF '"+B"' in e_integracao-ds_data_retorno WITH '"_B"'.
    REPLACE ALL OCCURRENCES OF '"+b"' in e_integracao-ds_data_retorno WITH '"_B"'.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = me->at_dados_retorno ).

    CLEAR: lva_msg.
    LOOP AT me->at_dados_retorno INTO DATA(lwa_fardo_retorno).

      IF strlen( lwa_fardo_retorno-id_filial_algodoeira ) NE 4.
        lva_msg = | Filial: { lwa_fardo_retorno-id_filial_algodoeira } com tamanho inválido! |.
      ELSEIF strlen( lwa_fardo_retorno-safra ) NE 4.
        lva_msg = | Safra: { lwa_fardo_retorno-safra } - Tamanho Inválido |.
      ELSEIF strlen( lwa_fardo_retorno-bloco ) NE 4.
        lva_msg = | Bloco: { lwa_fardo_retorno-bloco } - Tamanho Inválido |.
      ELSEIF strlen( lwa_fardo_retorno-nr_fardo ) > 10.
        lva_msg = | Nr.Fardo: { lwa_fardo_retorno-nr_fardo } - Tamanho Inválido |.
      ELSEIF strlen( lwa_fardo_retorno-nr_fardo_completo ) > 10.
        lva_msg = | Nr.Fardo Completo: { lwa_fardo_retorno-nr_fardo_completo } - Tamanho Inválido |.
      ENDIF.

      IF lva_msg IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lva_msg IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Erro Consulta Fardos Trace Cotton:'
                            attr2 = lva_msg )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Erro Consulta Fardos Trace Cotton:'
          msgv2  = lva_msg.


    ENDIF.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_DATA.

    r_if_integracao_outbound = me.

    CLEAR: e_data.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    e_referencia-tp_referencia = 'CONSULTA_FARDOS_TRACE'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_cons_fardo_trace_ct.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_outbound = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_url.

    DATA: lva_val         TYPE c,
          lva_index_bloco TYPE i,
          lva_url         TYPE string,
          lva_url_token   TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'CONSULTA_FARDOS_TRACE_COTTON'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'CONSULTA_FARDOS_TRACE_COTTON' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'CONSULTA_FARDOS_TRACE_COTTON'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = lwa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = zif_integracao_inject~co_request_method_get.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_inject~at_info_request_http-ds_url && 'Safras=' && me->at_dados_envio-safra && '&IdFilialAlgodoeira=' && me->at_dados_envio-filial_algodoeira.

    DATA(lva_qtde_blocos) = lines( me->at_dados_envio-blocos ).

    lva_index_bloco = 0.
    LOOP AT me->at_dados_envio-blocos INTO DATA(lwa_blocos).
      IF lva_qtde_blocos > 1.
        me->zif_integracao_inject~at_info_request_http-ds_url = |{ me->zif_integracao_inject~at_info_request_http-ds_url }&Blocos[{ lva_index_bloco }]={ lwa_blocos-bloco }|.
      ELSE.
        me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_inject~at_info_request_http-ds_url && '&Blocos=' && lwa_blocos-bloco.
      ENDIF.

      ADD 1 TO lva_index_bloco.
    ENDLOOP.

    IF me->at_dados_envio-bloco IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_inject~at_info_request_http-ds_url && '&Blocos=' && me->at_dados_envio-bloco.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
