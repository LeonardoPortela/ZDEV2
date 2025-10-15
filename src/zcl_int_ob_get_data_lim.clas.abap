class ZCL_INT_OB_GET_DATA_LIM definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '184' ##NO_TEXT.
  data AT_DADOS_ORDEM_ORC type ZPM_ORDEM_ORC .
  data AT_PARAMS type ZSTRUCT_GET_DATA_LIM .
  data AT_BODY type ZNFWE0001 .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_OB_GET_DATA_LIM IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'interface'.

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


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
*
*    r_if_integracao_inject = me.
*
*    TRY .
*        CAST zcl_integracao_token_ordem(
*               zcl_integracao_token_ordem=>zif_integracao_token_ordem~get_instance(
*                 )->get_token( )
*             )->zif_integracao_inject~get_header_request_http(
*          IMPORTING
*            e_header_fields = DATA(e_header_fields) ).
*
*        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).
*
*      CATCH zcx_error INTO DATA(ex_erro).
*
*        RAISE EXCEPTION TYPE zcx_integracao
*          EXPORTING
*            textid = VALUE #( msgid = ex_erro->zif_error~msgid
*                              msgno = ex_erro->zif_error~msgno
*                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
*                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
*                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
*                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
*            msgid  = ex_erro->zif_error~msgid
*            msgno  = ex_erro->zif_error~msgno
*            msgty  = 'E'
*            msgv1  = ex_erro->zif_error~msgv1
*            msgv2  = ex_erro->zif_error~msgv2
*            msgv3  = ex_erro->zif_error~msgv3
*            msgv4  = ex_erro->zif_error~msgv4.
*
*    ENDTRY.

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

*    LOOP AT me->at_dados_ordem_orc INTO DATA(lw_dados_ordem_orc).
*
*      UPDATE ZHCMT0007 SET int_sistemas_legado = abap_true
*                           dt_int_legado       = sy-datum
*                           hr_int_legado       = sy-uzeit
*       WHERE pernr EQ lwa_dados_funcinarios-pernr.
*
*    ENDLOOP.

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

   MOVE-CORRESPONDING i_info_request TO at_params.

  " ME->at_dados_ordem_orc = ls_ordem_orc.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~EXECUTE_REQUEST.

    R_IF_INTEGRACAO_OUTBOUND = ME.

    "Inclui Json na Mesagem a Ser Enviada
    ME->ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST( I_INFO_REQUEST = I_INFO_REQUEST
      )->GET_DATA( IMPORTING E_DATA = DATA(LC_DATA)
      )->SET_DATA( EXPORTING I_DATA = LC_DATA
      )->SET_URL(
      )->SET_ID_REFERENCIA(
      )->SEND_MSG( IMPORTING E_ID_INTEGRACAO = E_ID_INTEGRACAO E_INTEGRACAO  = E_INTEGRACAO
      ).

  endmethod.


  METHOD zif_integracao_outbound~get_data.

    CLEAR: e_data, at_body-instancia.

    at_body-instancia = at_params-id.

    r_if_integracao_outbound = me.


    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = me->at_body
      RECEIVING
        r_json = e_data.

    TRANSLATE e_data TO LOWER CASE.
    REPLACE all OCCURRENCES OF 'lim' in e_data WITH 'LIM'.


  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    "e_referencia-tp_referencia = 'ENV_FUNCIONARIOS_LEGADOS'.
    "e_referencia-id_referencia =

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE ZCL_INT_OB_GET_DATA_LIM.
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


  METHOD ZIF_INTEGRACAO_OUTBOUND~SET_URL.



*    DATA: lva_url       TYPE string,
*          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'GET_DATA_LIM'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'GET_DATA_LIM' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'GET_DATA_LIM'.
    ENDIF.

*    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    " Exemplo https://api.tblmanager.com/v1.0/indicator?api_token=2992e8379c8e6e936c60ae38931034ec6903e6b02f192c581ac5b5ae5ea30aee705654e3abb3d614
*    lva_url_token = lwa_webservice-url_token.


    DATA(v_url) = lwa_webservice-url." && at_params-endpoint.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url              = v_url.
*    me->zif_integracao_inject~at_info_request_http-ds_url_token        = lwa_webservice-token.
*    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = at_params-method.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_false.
*informação do header
    APPEND VALUE #( name = 'Authorization' value = lwa_webservice-token ) TO me->zif_integracao_inject~at_header_fields.
    "APPEND VALUE #( name = 'Content-Type'  value = 'application/x-www-form-urlencoded' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
