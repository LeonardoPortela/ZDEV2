class ZCL_INT_NF_ECOMM_XML definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '170' ##NO_TEXT.
  data AT_ID_CLIENTE_SAP type ZDE_ID_CLIENTE_SAP .
  data AT_SERVICE type /UI2/SERVICE_NAME value 'AMAGGION_ENVIA_NF_XML' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional .
  methods CRIAR_FATURA
    importing
      !IV_ECOMM type ZSDE_ECOMM_ID
      !IV_XML type STRING
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO .
  methods DELETA_FATURA
    importing
      !IV_ECOMM type ZSDE_ECOMM_ID
      !IV_NFENUM type J_1BNFNUM9
      !IV_XML type STRING
    returning
      value(RV_SUBRC) type SYSUBRC .
  methods ESTORNAR_FATURA
    importing
      !IV_ECOMM type ZSDE_ECOMM_ID
      !IV_NFENUM type J_1BNFNUM9
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO .
  PROTECTED SECTION.
private section.

  methods EXECUTE_SERVICE
    importing
      !I_SERVICE type /UI2/SERVICE_NAME default 'AMAGGION_CRIA_FATURA'
      !I_PARAMS type STRING
      !I_METHOD type ZDE_HTTP_METODO default 'POST'
      !I_BODY type STRING optional
      !I_FORMATO_BODY type ZDE_FORMATO_BODY default 'XML'
    returning
      value(R_INTEGRACAO) type ZINTEGRACAO .
ENDCLASS.



CLASS ZCL_INT_NF_ECOMM_XML IMPLEMENTATION.


  METHOD constructor.
    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
  ENDMETHOD.


  METHOD criar_fatura.

    DATA lv_params TYPE string.

    lv_params = iv_ecomm.

    r_integracao = me->execute_service( i_service = 'AMAGGION_CRIA_FATURA' i_params = lv_params i_body = iv_xml ).

  ENDMETHOD.


  METHOD deleta_fatura.

    DATA lv_params TYPE string.

    lv_params = iv_ecomm && '/' && iv_nfenum.

    TRY .

        DATA(lw_int) = me->execute_service( i_service = 'AMAGGION_DELETA_FATURA' i_params = lv_params ) . "i_body = iv_xml ).

        IF lw_int-nm_code < 206.
          rv_subrc = 0.
        ELSE.
          rv_subrc = 4.
        ENDIF.

      CATCH zcx_integracao .
      CATCH zcx_error .

    ENDTRY.

  ENDMETHOD.


  METHOD estornar_fatura.

    DATA lv_nfenum TYPE c LENGTH 10.
    DATA lv_params TYPE string.

    lv_nfenum = iv_nfenum.

    SHIFT lv_nfenum LEFT DELETING LEADING '0'.

    lv_params = iv_ecomm && '/' && lv_nfenum.

    r_integracao = me->execute_service( i_service = 'AMAGGION_DELETA_FATURA' i_params = lv_params ).

  ENDMETHOD.


  METHOD execute_service.

    DATA lc_integrar TYPE REF TO zcl_integracao.
    DATA lv_msgtx TYPE string.

    IF i_service IS NOT INITIAL.
      me->at_service = i_service.
    ENDIF.

    me->zif_integracao_outbound~set_url( ).

    IF me->zif_integracao_outbound~at_auth_webservice  IS INITIAL.
      lv_msgtx = `Serviço ` && me->at_service && ` não está configurado na ZAUTH_WEBSERVICE`.
      zcx_error=>zif_error~gera_erro_geral( i_texto = lv_msgtx ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato      = i_formato_body.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/xml'."me->zif_integracao_outbound~at_auth_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_integracao_outbound~at_auth_webservice-url && i_params.

    CLEAR me->zif_integracao_inject~at_multipart_fields.

    "me->zif_integracao_inject~at_info_request_http-ds_metodo = i_method.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_integracao_outbound~at_auth_webservice-method.

    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.

    CREATE OBJECT lc_integrar.

    IF i_body IS NOT INITIAL.
      me->zif_integracao_inject~at_info_request_http-ds_body = i_body.
    ELSE.
      CLEAR me->zif_integracao_inject~at_info_request_http-ds_body.
    ENDIF.

*****    "TRY.
*****
*****    CALL METHOD me->zif_integracao_outbound~send_msg
*****      IMPORTING
*****        e_id_integracao = DATA(e_id_integracao)
*****        e_integracao    = r_integracao.
*****
*****    "ENDTRY.

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


  METHOD zif_integracao_inject~get_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.

    TRY .
        CAST zcl_int_ob_token_ecommerce(
               zcl_int_ob_token_ecommerce=>zif_int_ob_token_ecommerce~get_instance(
                 )->set_usuario_senha( "EXPORTING I_SENHA = ME->ZIF_INTEGRACAO_COF_KUHLMANN~AT_SENHA
                                       "          I_USUARIO = ME->ZIF_INTEGRACAO_COF_KUHLMANN~AT_USUARIO
                 )
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

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  METHOD zif_integracao_outbound~build_info_request.

    r_if_integracao_outbound = me.
    me->at_id_cliente_sap = i_info_request.

  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

    r_if_integracao_outbound = me.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_outbound~build_info_request( i_info_request = i_info_request
      )->get_data( IMPORTING e_data = DATA(lc_data)
      )->set_data( EXPORTING i_data = lc_data
      )->set_url(
      )->set_id_referencia(
      )->send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_data.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = me->at_id_cliente_sap
      RECEIVING
        r_json = e_data.


  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound = me.
    e_referencia-tp_referencia = 'ECOMMERCE_INT_NF_XML'.
    e_referencia-id_referencia = me->at_id_cliente_sap.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_nf_ecomm_xml.
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
             FROM zauth_webservice
             INTO @DATA(lwa_webservice)
            WHERE service = @me->at_service . "'AMAGGION_ENVIA_NF_XML'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = me->at_service )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = CONV #( me->at_service ).
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.


    lva_url_token = lwa_webservice-url_token.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'XML'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_webservice-content_type.

    me->zif_integracao_inject~at_info_request_http-ds_url_token        = lva_url_token.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.


    me->zif_integracao_inject~at_info_request_http-ds_metodo           = lwa_webservice-method.
    me->zif_integracao_inject~at_info_request_http-ds_url              = lva_url.

    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


  ENDMETHOD.
ENDCLASS.
