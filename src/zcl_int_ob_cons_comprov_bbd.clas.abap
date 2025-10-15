class ZCL_INT_OB_CONS_COMPROV_BBD definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '307' ##NO_TEXT.
  data AT_IDTRANSACAO type STRING .
  data AT_E2E type STRING .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_INT_OB_CONS_COMPROV_BBD IMPLEMENTATION.


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

    r_if_integracao_inject = me.

*    TRY .
*        CAST zcl_integracao_token_coupa(
*                zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
*                  )->get_token( )
*              )->zif_integracao_inject~get_header_request_http(
*           IMPORTING
*             e_header_fields = DATA(e_header_fields) ).
*
*        APPEND VALUE #( name = 'accept'   value = 'application/json' ) TO e_header_fields.
*
*        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).
*
*
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


  METHOD ZIF_INTEGRACAO_OUTBOUND~BUILD_INFO_REQUEST.

    r_if_integracao_outbound = me.

    SPLIT i_info_request AT ';' INTO me->at_idtransacao me->at_e2e .

  ENDMETHOD.


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


  METHOD zif_integracao_outbound~get_data.
    r_if_integracao_outbound = me.
*    TYPES: BEGIN OF ty_cnpj,
*             cnpj_empresa TYPE string,
*           END OF ty_cnpj.
*
*    DATA: ls_cnpj TYPE ty_cnpj.
*
*    r_if_integracao_outbound = me.
*
*    CLEAR: e_data.
*
*    ls_cnpj-cnpj_empresa = me->at_cnpj.
*
*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data   = ls_cnpj
*      RECEIVING
*        r_json = e_data.
*
*    TRANSLATE e_data to LOWER CASE.

  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    "e_referencia-tp_referencia = 'ENV_FUNCIONARIOS_LEGADOS'.
    "e_referencia-id_referencia =

  endmethod.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_cons_comprov_bbd.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    DATA: LC_INTEGRAR TYPE REF TO ZCL_INTEGRACAO.

    R_IF_INTEGRACAO_OUTBOUND = ME.

    CREATE OBJECT LC_INTEGRAR.

*    APPEND VALUE #( name = 'accept'   value = 'application/json' ) TO LC_INTEGRAR~at_.

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

    DATA: lva_url       TYPE string,
          lva_url_token TYPE string.

    CONSTANTS: lc_authorization TYPE string VALUE 'Authorization',
               lc_basic         TYPE string VALUE 'Basic'.

    DATA: zcl_base64encoder TYPE REF TO cl_http_utility.
    CREATE OBJECT zcl_base64encoder.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'CONSULTA_COMPROVANTES_PIX_BBD'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'CONSULTA_COMPROVANTES_PIX_BBD' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'CONSULTA_COMPROVANTES_PIX_BBD'.
    ENDIF.

    "CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-' sy-datum+6(2) INTO DATA(lv_data).

    CONCATENATE lwa_webservice-url '/' me->at_idtransacao '/' me->at_e2e INTO lva_url.

    FREE: me->zif_integracao_inject~at_header_fields.

    DATA(lv_tpcredentials) = zcl_base64encoder->if_http_utility~encode_base64( |{ lwa_webservice-token }| ).

    APPEND VALUE #( name = lc_authorization value = |{ lc_basic } { lv_tpcredentials }| ) TO me->zif_integracao_inject~at_header_fields.

*    if_rest_message=>gc_method_post = 'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_formato          = 'JSON'.
*    me->zif_integracao_inject~at_info_request_http-ds_content_type     = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url              = lva_url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = lwa_webservice-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.
ENDCLASS.
