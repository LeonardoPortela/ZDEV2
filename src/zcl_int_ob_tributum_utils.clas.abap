class ZCL_INT_OB_TRIBUTUM_UTILS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '224' ##NO_TEXT.
  data AT_DADOS_REQUISICAO type ZMMS_REQUISICAO_TRIBUTUM .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  class-methods FORCE_DOWNLOAD_NFE
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
    returning
      value(R_CHAVE_INTEGRADA) type CHAR01 .
  PROTECTED SECTION.
private section.

  data AT_CLIENTE_ID type STRING value 'amaggi' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_INT_OB_TRIBUTUM_UTILS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.


  ENDMETHOD.


  METHOD force_download_nfe.

    DATA: lwa_req_tributum TYPE zmms_requisicao_tributum.

    DATA: lwa_nfe TYPE zmms_response_tributum_002.

    CLEAR: r_chave_integrada.

    CHECK i_chave_nfe IS NOT INITIAL.

*-------------------------------------------------------------------------------------------------------------*
*   Buscar Codigo de Download da NF-e no Tributum
*-------------------------------------------------------------------------------------------------------------*
    SELECT SINGLE *
      FROM zib_nfe_dist_ter INTO @DATA(lwa_zib_nfe_dist_ter)
     WHERE chave_nfe EQ @i_chave_nfe.

    CHECK sy-subrc NE 0.

    CLEAR: lwa_req_tributum.
    lwa_req_tributum-tp_requisicao           = '04'. "Pegar NFe
    lwa_req_tributum-pegar_nfe-nfe_chave     = i_chave_nfe.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum
                                                                                             IMPORTING e_integracao = DATA(lwa_integracao) ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lwa_integracao-ds_data_retorno CHANGING data = lwa_nfe ).

      CATCH zcx_integracao.
      CATCH zcx_error.
    ENDTRY.

*-------------------------------------------------------------------------------------------------------------*
*   Se conseguiu obter o Codigo de Download da NF-e no Tributum, baixar o XML e criar o EDocument
*-------------------------------------------------------------------------------------------------------------*
    CHECK  lwa_nfe-codigo_download IS NOT INITIAL.

    CLEAR: lwa_req_tributum.
    lwa_req_tributum-tp_requisicao                      = '03'. "Download do XML da NFe
    lwa_req_tributum-download_xml_nfe-codigo_download   = lwa_nfe-codigo_download.

    TRY.
        zcl_int_ob_tributum_utils=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lwa_req_tributum IMPORTING e_integracao = lwa_integracao ).

        CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

        DATA(_xml_base64) = zcl_string=>string_to_base64( i_texto = lwa_integracao-ds_data_retorno ).

        "Criar EDocument
        CALL FUNCTION 'ZEDOC_BR_RECEIVE_XML'
          EXPORTING
            iv_xml         = _xml_base64
          EXCEPTIONS
            write_error    = 1
            log_save_error = 2
            OTHERS         = 3.

        WAIT UP TO 1 SECONDS.

      CATCH zcx_integracao.
      CATCH zcx_error.
    ENDTRY.

*-------------------------------------------------------------------------------------------------------------*
*   Se conseguiu criar o Edocument, forçar integração com a ZFIS25
*-------------------------------------------------------------------------------------------------------------*
    SELECT SINGLE accesskey
      FROM edobrincoming INTO @DATA(lwa_edobrincoming)
     WHERE accesskey EQ @i_chave_nfe.

    CHECK sy-subrc EQ 0.

    SUBMIT zdrcr_transf_edoc_to_zib WITH pchave = i_chave_nfe AND RETURN.

    WAIT UP TO 1 SECONDS.

    SELECT SINGLE *
      FROM zib_nfe_forn INTO @DATA(lwa_zib_nfe_forn)
     WHERE nu_chave EQ @i_chave_nfe.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zib_nfe_dist_ter INTO lwa_zib_nfe_dist_ter
     WHERE chave_nfe EQ i_chave_nfe.

    CHECK sy-subrc EQ 0.

    r_chave_integrada = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    CHECK me->zif_integracao_inject~at_header_fields IS INITIAL.

    TRY.

        CAST zcl_int_ob_token_tributum(
               zcl_int_ob_token_tributum=>zif_integracao_outbound~get_instance(
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


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.

    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST.

   R_IF_INTEGRACAO_OUTBOUND = ME.

   MOVE-CORRESPONDING i_info_request TO ME->at_dados_requisicao.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~EXECUTE_REQUEST.

    R_IF_INTEGRACAO_OUTBOUND = ME.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST( I_INFO_REQUEST = I_INFO_REQUEST
      )->GET_DATA( IMPORTING E_DATA = DATA(LC_DATA)
      )->SET_DATA( EXPORTING I_DATA = LC_DATA
      )->SET_URL(
      )->SET_ID_REFERENCIA(
      )->SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO E_INTEGRACAO	= E_INTEGRACAO
      ).

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_DATA.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

    CASE ME->at_dados_requisicao-tp_requisicao.
      WHEN '01'.  "Lista Empresas
      WHEN '02'.  "Lista NF-es
      WHEN '03'.  "Download do XML da NFe
      WHEN '04'.  "Pegar NFe
      WHEN '05'.  "Listar Eventos da NFe
      WHEN '06'.  "Download XML do Evento de NFe
      WHEN '07'.  "Listar CTes
      WHEN '08'.  "Download XML do CTe
      WHEN '09'.  "Pegar CTe
      WHEN '10'.  "Listar Eventos do CTe
      WHEN '11'.  "Download XML do Evento de CTe
      WHEN '12'.  "Lista Eventos NF-e por CNPJ e Data
      WHEN '13'.  "Lista Eventos CT-e por CNPJ e Data

    ENDCASE.

*    CALL METHOD /ui2/cl_json=>serialize
*       EXPORTING
*         data   = me->at_dados_due
*       RECEIVING
*         r_json = e_data.


  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    e_referencia-tp_referencia = 'TRIBUTUM'.
    "e_referencia-id_referencia = .

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_tributum_utils.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_OUTBOUND = ME.

    CREATE OBJECT LC_INTEGRAR.

    "Cria MSG para Integração via HTTP
    LC_INTEGRAR->ZIF_INTEGRACAO~SET_MSG_INJECT( I_MSG = CAST #( ME )
      )->SET_NEW_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO
      )->SET_OUTBOUND_MSG(
      )->SET_PROCESSAR_RETORNO(
      )->SET_INTEGRAR_RETORNO(
      )->GET_REGISTRO( IMPORTING E_INTEGRACAO = E_INTEGRACAO
      )->FREE(
      ).

    CLEAR: LC_INTEGRAR.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = ME.

    ME->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  METHOD zif_integracao_outbound~set_url.

    DATA: lva_val       TYPE c,
          lva_url       TYPE string,
          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'TRIBUTUM_HOST'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'TRIBUTUM_HOST' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'TRIBUTUM_HOST'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_info_request_http-ds_url = lwa_webservice-url.

    CASE me->at_dados_requisicao-tp_requisicao.
      WHEN '01'.  "Lista Empresas
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' && me->at_cliente_id && '/empresas/'.
      WHEN '02'.  "Lista NF-es
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/'
                                                                     && me->at_cliente_id &&
                                                                     '/nfes/?'        &&
                                                                     'cnpj='          && me->at_dados_requisicao-lista_nfes-cnpj &&
                                                                     '&data_inicial=' && me->at_dados_requisicao-lista_nfes-data_inicial &&
                                                                     '&data_final='   && me->at_dados_requisicao-lista_nfes-data_final &&
                                                                     '&tipo_data='    && me->at_dados_requisicao-lista_nfes-tipo_data &&
                                                                     '&tipo_nfe='     && me->at_dados_requisicao-lista_nfes-tipo_nfe.

      WHEN '03'.  "Download do XML da NFe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/nfes/' &&
                                                                     me->at_dados_requisicao-download_xml_nfe-codigo_download &&
                                                                    '/download_xml'.
      WHEN '04'.  "Pegar NFe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/nfes/' &&
                                                                    me->at_dados_requisicao-pegar_nfe-nfe_chave.
      WHEN '05'.  "Listar Eventos da NFe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/proceventonfes/' &&
                                                                    me->at_dados_requisicao-lista_eventos_nfe-nfe_chave.
      WHEN '06'.  "Download XML do Evento de NFe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/proceventonfes/' &&
                                                                    me->at_dados_requisicao-download_xml_evento_nfe-codigo_download &&
                                                                    '/download_xml'.
      WHEN '07'.  "Listar CTes
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/ctes/?'        &&
                                                                    'cnpj='          && me->at_dados_requisicao-lista_ctes-cnpj &&
                                                                    '&data_inicial=' && me->at_dados_requisicao-lista_ctes-data_inicial &&
                                                                    '&data_final='   && me->at_dados_requisicao-lista_ctes-data_final &&
                                                                    '&tipo_data='    && me->at_dados_requisicao-lista_ctes-tipo_data &&
                                                                    '&tipo_cte='     && me->at_dados_requisicao-lista_ctes-tipo_cte.
      WHEN '08'.  "Download XML do CTe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/ctes/' &&
                                                                    me->at_dados_requisicao-download_xml_cte-codigo_download &&
                                                                    '/download_xml'.
      WHEN '09'.  "Pegar CTe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/ctes/' &&
                                                                    me->at_dados_requisicao-pegar_cte-cte_chave.
      WHEN '10'.  "Listar Eventos do CTe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/proceventoctes/' &&
                                                                    me->at_dados_requisicao-lista_eventos_cte-cte_chave.
      WHEN '11'.  "Download XML do Evento de CTe
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/' &&
                                                                    me->at_cliente_id &&
                                                                    '/proceventoctes/' &&
                                                                    me->at_dados_requisicao-download_xml_evento_cte-codigo_download &&
                                                                    '/download_xml'.

      WHEN '12'.  "Lista Eventos NF-es por CNPJ e Data
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/'
                                                                     && me->at_cliente_id &&
                                                                     '/proceventonfes/?'        &&
                                                                     'cnpj='          && me->at_dados_requisicao-lista_nfes_eventos-cnpj &&
                                                                     '&data_inicial=' && me->at_dados_requisicao-lista_nfes_eventos-data_inicial &&
                                                                     '&data_final='   && me->at_dados_requisicao-lista_nfes_eventos-data_final &&
                                                                     '&tipo_data='    && me->at_dados_requisicao-lista_nfes_eventos-tipo_data.
      WHEN '13'.  "Lista Eventos CT-es por CNPJ e Data
        me->zif_integracao_inject~at_info_request_http-ds_metodo  = me->zif_integracao_inject~co_request_method_get.
        me->zif_integracao_inject~at_info_request_http-ds_url     = me->zif_integracao_inject~at_info_request_http-ds_url && '/clientes/'
                                                                     && me->at_cliente_id &&
                                                                     '/proceventoctes/?'        &&
                                                                     'cnpj='          && me->at_dados_requisicao-lista_ctes_eventos-cnpj &&
                                                                     '&data_inicial=' && me->at_dados_requisicao-lista_ctes_eventos-data_inicial &&
                                                                     '&data_final='   && me->at_dados_requisicao-lista_ctes_eventos-data_final &&
                                                                     '&tipo_data='    && me->at_dados_requisicao-lista_ctes_eventos-tipo_data.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
