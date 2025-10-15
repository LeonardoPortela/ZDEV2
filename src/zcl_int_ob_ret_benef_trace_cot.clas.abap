CLASS zcl_int_ob_ret_benef_trace_cot DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_outbound .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '034' ##NO_TEXT.
    DATA at_dados_retorno TYPE zpps0005 .
    DATA at_zppt0002_retorno TYPE zppt0002_t .
    DATA at_zppt0006_retorno TYPE zppt0006_t .

    METHODS constructor
      IMPORTING
        VALUE(i_servico) TYPE ztipowebserv OPTIONAL
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
private section.

  methods VALIDA_ENVIO
    importing
      !I_INFO_REQUEST type ANY
    returning
      value(R_VALIDADO) type CHAR01 .
  methods CHECK_ENVIO_EMAIL_ALERTA
    importing
      !I_MSG_ERROR type STRING .
ENDCLASS.



CLASS ZCL_INT_OB_RET_BENEF_TRACE_COT IMPLEMENTATION.


  METHOD check_envio_email_alerta.

    DATA: lva_data_ultimo_email       TYPE char14,
          lva_qtde_tentativas_retorno TYPE i,
          lva_protocolo               TYPE zppt0002-id_referencia,
          lva_texto_corpo             TYPE string,
          lv_title_email              TYPE string.

    DATA(lva_envia_email_alerta) = abap_false.
    CLEAR: lva_qtde_tentativas_retorno.

    LOOP AT me->at_zppt0002_retorno ASSIGNING FIELD-SYMBOL(<fs_zppt0002>).

      ADD 1 TO <fs_zppt0002>-qtd_tentativa_ret_sis_orig.

      UPDATE zppt0002 SET erro_ret_sistema_origem    = abap_true
                          qtd_tentativa_ret_sis_orig = <fs_zppt0002>-qtd_tentativa_ret_sis_orig
        WHERE acharg     EQ <fs_zppt0002>-acharg
          AND werks      EQ <fs_zppt0002>-werks
          AND id_sessao  EQ <fs_zppt0002>-id_sessao
          AND lgort      EQ <fs_zppt0002>-lgort
          AND cd_safra   EQ <fs_zppt0002>-cd_safra.

      lva_qtde_tentativas_retorno               = <fs_zppt0002>-qtd_tentativa_ret_sis_orig.
      lva_protocolo                             = <fs_zppt0002>-id_referencia.

      COMMIT WORK.

    ENDLOOP.

    CHECK lva_qtde_tentativas_retorno >= 10.

    lv_title_email = |SAP { sy-sysid } - RETORNO PROCESSAMENTO BENEFICIAMENTO - TRACE COTTON|.
    lva_texto_corpo = |Houve falha de retorno no processamento do Beneficiamento para o sistema Trace Cotton! Verificar Log Transação ZWS0004 - Id Interface: 034 e Id. Referencia: { lva_protocolo } |.

    zcl_trace_cotton_utils=>disparar_email_alerta(
      i_zppt0002_t        = me->at_zppt0002_retorno
      i_titulo_email      = lv_title_email
      i_texto_corpo_email = lva_texto_corpo
      i_msg_error         = i_msg_error
    ).

  ENDMETHOD.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.


  ENDMETHOD.


  METHOD valida_envio.

    CLEAR: r_validado.

    DATA: lwa_recebimento        TYPE zpps0006.
    DATA: lit_zppt0002_retorno   TYPE TABLE of zppt0002.

    MOVE-CORRESPONDING i_info_request TO lwa_recebimento.

    CHECK lwa_recebimento-protocolo_recebimento IS NOT INITIAL.

    SELECT *
      FROM zppt0002 INTO TABLE me->at_zppt0002_retorno
     WHERE id_referencia EQ lwa_recebimento-protocolo_recebimento.

    CHECK me->at_zppt0002_retorno[] IS NOT INITIAL.

    SELECT *
      FROM zppt0006 INTO TABLE me->at_zppt0006_retorno
     WHERE id_referencia2  = lwa_recebimento-protocolo_recebimento.

    CHECK me->at_zppt0006_retorno[] IS NOT INITIAL.

    LOOP AT me->at_zppt0002_retorno INTO DATA(lwa_retorno) WHERE status_processamento NE 'C'. "Registros com processamento nao concluido
      RETURN.
    ENDLOOP.

    "Verificar se tem alguma movimentação da filial(Algodoeira) e safra, anterior a essa movimentação e que não retornou para o Trace...
    "Se existir, aguardar retorno das movimentações anteriores para o trace
    SELECT SINGLE *
      from tvarvc INTO @DATA(lwa_tvarvc_ck)
     WHERE name = 'ENABLE_RET_CRONOLOGICO_TRACE'.
    IF SY-SUBRC EQ 0.
      READ TABLE me->at_zppt0002_retorno INTO DATA(lwa_zppt0002_check) INDEX 1.
      IF sy-subrc EQ 0.
        CLEAR: lit_zppt0002_retorno[].
        SELECT *
          FROM zppt0002 INTO TABLE lit_zppt0002_retorno
         WHERE werks                      EQ lwa_zppt0002_check-werks
           AND cd_safra                   EQ lwa_zppt0002_check-cd_safra
           AND status_ret_sistema_origem  EQ 'P'. "Pendente

        LOOP AT lit_zppt0002_retorno INTO lwa_retorno WHERE id_referencia LT lwa_zppt0002_check-id_referencia
                                                        AND id_referencia IS NOT INITIAL.
          RETURN.
        ENDLOOP.
      ENDIF.
    ENDIF.

    r_validado = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

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


  METHOD zif_integracao_inject~set_form_request_http.
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

    DATA: lit_zppt0006 TYPE TABLE OF zppt0006,
          lit_zppt0002 TYPE TABLE OF zppt0002.

    DATA: lwa_retorno  TYPE zpps0009.

    r_if_integracao_inject = me    .
    e_sucesso = abap_true.

    CLEAR: lwa_retorno.
    /ui2/cl_json=>deserialize( EXPORTING json =   CONV #( i_msg_retorno ) CHANGING data = lwa_retorno ).

    LOOP AT me->at_zppt0002_retorno INTO DATA(lwa_zppt0002).

      UPDATE zppt0002 SET status_ret_sistema_origem    = 'C' "Retorno Realizado
                          prot_retorno_sistema_origem  = lwa_retorno-protocolo
        WHERE acharg     EQ lwa_zppt0002-acharg
          AND werks      EQ lwa_zppt0002-werks
          AND id_sessao  EQ lwa_zppt0002-id_sessao
          AND lgort      EQ lwa_zppt0002-lgort
          AND cd_safra   EQ lwa_zppt0002-cd_safra.

      COMMIT WORK.

    ENDLOOP.

    LOOP AT me->at_zppt0006_retorno INTO DATA(lwa_zppt0006).
      lwa_zppt0006-id_referencia                = me->at_dados_retorno-dadosgerais-protocolo.
      lwa_zppt0006-flag_envio                   = 'P'.
      lwa_zppt0006-prot_retorno_sistema_origem  = lwa_retorno-protocolo.
      MODIFY zppt0006 FROM lwa_zppt0006.
    ENDLOOP.

    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_outbound~build_info_request.

    r_if_integracao_outbound = me.

    DATA: lwa_dados_detalhamento TYPE zpps0005_002.

    CLEAR: me->at_dados_retorno.

    READ TABLE  me->at_zppt0002_retorno INTO DATA(lwa_zppt0002_header) INDEX 1.

    me->at_dados_retorno-dadosgerais-protocolo               = lwa_zppt0002_header-id_referencia.
    me->at_dados_retorno-dadosgerais-id_sessao               = lwa_zppt0002_header-id_sessao.
    me->at_dados_retorno-dadosgerais-id_filial_algodoeira    = lwa_zppt0002_header-werks.
    me->at_dados_retorno-dadosgerais-safra                   = lwa_zppt0002_header-cd_safra.
    me->at_dados_retorno-dadosgerais-id_movimentacao         = lwa_zppt0002_header-id_mov_sistema_origem.

    DATA(lit_zppt0002_ret) = me->at_zppt0002_retorno[].

    CASE lwa_zppt0002_header-status_registro.
      WHEN '06' OR 'E6'.
        DELETE lit_zppt0002_ret WHERE lgort IS NOT INITIAL.
    ENDCASE.

    LOOP AT lit_zppt0002_ret INTO DATA(lwa_zppt0002_det).

      CLEAR: lwa_dados_detalhamento.

      IF lwa_zppt0002_det-bloco_destino IS NOT INITIAL.
        lwa_dados_detalhamento-bloco              = lwa_zppt0002_det-bloco_destino.
      ELSE.
        lwa_dados_detalhamento-bloco              = lwa_zppt0002_det-lgort.
      ENDIF.

      lwa_dados_detalhamento-doc_material       = lwa_zppt0002_det-mblnr.
      lwa_dados_detalhamento-doc_material_ano   = lwa_zppt0002_det-mjahr.
      lwa_dados_detalhamento-tp_integracao      = lwa_zppt0002_det-status_registro.
      lwa_dados_detalhamento-mensagem           = lwa_zppt0002_det-cd_mensagem.

      APPEND lwa_dados_detalhamento TO me->at_dados_retorno-detalhamento.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

    DATA: lva_msg_error                 TYPE string,
          lva_data_limite               TYPE char14,
          lva_data_ultimo_email         TYPE char14,
          lva_qtde_tentativas_retorno   TYPE i,
          lva_ret_date                  TYPE tvpod-rudat,
          lva_ret_time                  TYPE tvpod-rutim,
          lva_duration_integer          TYPE i.

    r_if_integracao_outbound = me.

    DATA(_validado) = me->valida_envio( i_info_request =  i_info_request ).

    CHECK _validado = abap_true.

    TRY.
        CLEAR: lva_msg_error.

        "Inclui Json na Mesagem a Ser Enviada
        me->zif_integracao_outbound~build_info_request( i_info_request = i_info_request
          )->get_data( IMPORTING e_data = DATA(lc_data)
          )->set_data( EXPORTING i_data = lc_data
          )->set_url(
          )->set_id_referencia(
          )->send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
          ).

      CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
        MESSAGE ID lwa_zcx_integracao->msgid TYPE 'S' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4 INTO lva_msg_error.
      CATCH zcx_error INTO DATA(zcx_error).
        MESSAGE ID zcx_error->msgid TYPE 'S' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4 INTO lva_msg_error.
    ENDTRY.

    CHECK lva_msg_error IS NOT INITIAL.

    me->check_envio_email_alerta( i_msg_error = lva_msg_error ).


  endmethod.


  METHOD zif_integracao_outbound~get_data.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = me->at_dados_retorno
      RECEIVING
        r_json = e_data.


  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound = me.
    e_referencia-tp_referencia = 'BENEFICIAMENTO_ALG_RESPONSE'.
    e_referencia-id_referencia = me->at_dados_retorno-dadosgerais-protocolo.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_ret_benef_trace_cot.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  METHOD zif_integracao_outbound~send_msg.

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


  METHOD zif_integracao_outbound~set_data.

    r_if_integracao_outbound = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_id_referencia.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_url.

    DATA: lva_val       TYPE c,
          lva_url       TYPE string,
          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'ENVIA_TRACE_ATUALIZA'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'ENVIA_TRACE_ATUALIZA' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'ENVIA_TRACE_ATUALIZA'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = lwa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = me->zif_integracao_inject~co_request_method_post.


  ENDMETHOD.
ENDCLASS.
