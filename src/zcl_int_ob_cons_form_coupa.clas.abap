class ZCL_INT_OB_CONS_FORM_COUPA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '192' ##NO_TEXT.
  data AT_DADOS_ORDEM_ORC type ZPM_ORDEM_ORC .
  data AT_REQ type STRING .
  data AT_FORM type ZMMT0171 .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_INT_OB_CONS_FORM_COUPA IMPLEMENTATION.


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

**    CHECK me->zif_integracao_inject~at_header_fields IS INITIAL.

    TRY .

        " debug
*        IF sy-uname = 'AOENNING' OR sy-sysid = 'DEV'.
          APPEND VALUE #( name = 'x-coupa-api-key' value = 'be82ead986a55cbb33155ab2c7661641307a8b8b' ) TO me->zif_integracao_inject~at_header_fields.
*        ELSE.

          CAST zcl_integracao_token_coupa(
                 zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                   )->get_token( )
               )->zif_integracao_inject~get_header_request_http(
            IMPORTING
              e_header_fields = DATA(e_header_fields) ).


          me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).
*        ENDIF.

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

    APPEND VALUE #( name = 'Accept' value = 'application/json' ) TO me->zif_integracao_inject~at_header_fields.

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

    DATA: ls_dados_form TYPE zmmt0171.

    r_if_integracao_outbound = me.

    MOVE-CORRESPONDING i_info_request TO  ls_dados_form.

    me->at_form = ls_dados_form.

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

    DATA: zbody TYPE string.

    r_if_integracao_outbound = me.





    CLEAR: e_data.
    IF me->at_form-zmsg_rej IS NOT INITIAL.
      e_data = '{ "reason": ' && '"' && me->at_form-zmsg_rej && '"' && '}'.

*      CALL METHOD /ui2/cl_json=>serialize
*        EXPORTING
*          data   = me->at_form-zmsg_rej
*        RECEIVING
*          r_json = e_data.

    ENDIF.

    IF me->at_form-json_pram IS NOT INITIAL.
      e_data = me->at_form-json_pram.
    ENDIF.


  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    "e_referencia-tp_referencia = 'ENV_FUNCIONARIOS_LEGADOS'.
    "e_referencia-id_referencia =

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_cons_form_coupa.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  endmethod.


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

    DATA: lva_url         TYPE string,
          vg_param        TYPE string,
          lva_url_token   TYPE string,
          zvg_method      TYPE string VALUE 'GET',
          zvg_param_url   TYPE string,
          zvg_param_url_1 TYPE string,
          zvg_param_url_2 TYPE string,
          ws_dados        TYPE zmmt0171.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = @me->at_form-para_serv_url.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = ws_dados-para_serv_url )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = CONV #( ws_dados-para_serv_url ).
    ENDIF.

    CLEAR: vg_param, zvg_param_url, zvg_param_url_1, zvg_param_url_2.
    CLEAR: ws_dados .
    MOVE-CORRESPONDING me->at_form TO ws_dados.

    CASE ws_dados-zcheck_api.
      WHEN 1.
        "Parametro API para seleção dos formularios aprovados.
        vg_param = me->at_form-id_ativa_forn.
        vg_param = |{ vg_param ALPHA = OUT }|.
        CONDENSE vg_param NO-GAPS.

        REPLACE ALL OCCURRENCES OF '#' IN ws_dados-zparam_url_form WITH vg_param.
        lva_url = |{ lwa_webservice-url }/{ ws_dados-zparam_url_form }{ ws_dados-zoffset }|.

      WHEN 2.
        "Parametro API para seleção dos formularios reprovado.
        vg_param = me->at_form-id_desat_forn.
        vg_param = |{ vg_param ALPHA = OUT }|.
        CONDENSE vg_param NO-GAPS.

        "Parametro API para seleção dos formularios reprovados.
        REPLACE ALL OCCURRENCES OF '#' IN ws_dados-zparam_url_form WITH vg_param.
        lva_url = |{ lwa_webservice-url }/{ ws_dados-zparam_url_form }{ ws_dados-zoffset }|.

      WHEN 3.
        "Parametro API para seleção dos dados fornecedor com base no formulario selecionado.
        vg_param = me->at_form-id_formulario.
        vg_param = |{ vg_param ALPHA = OUT }|.
        CONDENSE vg_param NO-GAPS.

        REPLACE ALL OCCURRENCES OF '#' IN ws_dados-zparam_url_detalh WITH vg_param.
        lva_url = |{ lwa_webservice-url }/{ ws_dados-zparam_url_detalh }|.
      WHEN 4.
        "Parametro API enviar o status de processamento do formulario aprovado.
        vg_param = me->at_form-id_formulario.
        vg_param = |{ vg_param ALPHA = OUT }|.
        CONDENSE vg_param NO-GAPS.

        REPLACE ALL OCCURRENCES OF '#' IN ws_dados-zparam_url_put_aprov WITH vg_param.
        zvg_method = 'PUT'.
        lva_url = |{ lwa_webservice-url }/{ ws_dados-zparam_url_put_aprov }|.

      WHEN 5.

        vg_param = me->at_form-id_formulario.
        vg_param = |{ vg_param ALPHA = OUT }|.
        CONDENSE vg_param NO-GAPS.
        REPLACE ALL OCCURRENCES OF '#' IN ws_dados-zparam_url_put_rej WITH vg_param.
        zvg_method = 'PUT'.
        lva_url = |{ lwa_webservice-url }/{ ws_dados-zparam_url_put_rej }|.


      WHEN 6.

        vg_param = me->at_form-id_formulario.
        vg_param = |{ vg_param ALPHA = OUT }|.
        CONDENSE vg_param NO-GAPS.

        REPLACE ALL OCCURRENCES OF '#' IN ws_dados-zparam_url_get_cliente WITH vg_param.
        lva_url = |{ lwa_webservice-url }/{ ws_dados-zparam_url_get_cliente }|.

      WHEN 7. "Consultr lista LOOKUP
        REPLACE ALL OCCURRENCES OF '#' IN lwa_webservice-url WITH me->at_form-id_param.
        lva_url = lwa_webservice-url.
        zvg_method = 'GET'.

      WHEN 8. "Adicionar fornecedor na lista LOOPUP - Montar URL.
        lva_url = lwa_webservice-url.
        zvg_method = 'POST'.

      WHEN 9. "Adicionar fornecedor na lista LOOPUP - Montar URL.
        REPLACE ALL OCCURRENCES OF '#' IN lwa_webservice-url WITH me->at_form-id_param.
        lva_url = lwa_webservice-url.
        zvg_method = 'PUT'.

      WHEN OTHERS.
    ENDCASE.

    TRANSLATE lva_url TO LOWER CASE.
    CONDENSE lva_url NO-GAPS.

    FREE: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'accept'   value = 'application/json' ) TO me->zif_integracao_inject~at_header_fields.

*    if_rest_message=>gc_method_post = 'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
*    me->zif_integracao_inject~at_info_request_http-ds_content_type      = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = lva_url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = zvg_method.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.
ENDCLASS.
