class ZCL_INT_OB_ACTIVE_DIRECTORY_V2 definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '177' ##NO_TEXT.
  data AT_GET_USR type ZDE_GET_USR .
  data AT_RESULT_USR type STRING .
  data AT_GET_ACTIVE_DIRECTORY type ZDE_ACTIVE_DIRECTORY .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
protected section.
private section.

  data AT_SET_USR_BLOQ type ZDE_SET_USR_BLOQ .
ENDCLASS.



CLASS ZCL_INT_OB_ACTIVE_DIRECTORY_V2 IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'activedirectory'.

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
    r_if_integracao_inject = me.

    "DATA: lwa_retorno_get_usr TYPE zde_due_sol_mod_retorno.

    /ui2/cl_json=>deserialize( EXPORTING json = i_msg_retorno CHANGING data = me->at_result_usr ).

    e_sucesso = abap_true.

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

    DATA: ls_active_directory TYPE zde_active_directory.

    r_if_integracao_outbound = me.
    MOVE-CORRESPONDING i_info_request TO ls_active_directory.

    me->at_get_active_directory = ls_active_directory.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~EXECUTE_REQUEST.

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

  ENDMETHOD.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.
    r_if_integracao_outbound = me.
  endmethod.


  METHOD ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_active_directory.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_OUTBOUND~SEND_MSG.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_outbound = me.

    DATA(lwa_data) = me->at_get_active_directory.

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

    r_if_integracao_outbound = me.

    IF  me->at_get_active_directory IS NOT INITIAL.
      DATA(lwa_data) = me->at_get_active_directory.
    ENDIF.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'INTEGRA_ACTIVE_DIRECTORY'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'INTEGRA_ACTIVE_DIRECTORY' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'INTEGRA_ACTIVE_DIRECTORY'.
    ENDIF.

    CASE lwa_data-tipo.
      WHEN 'GET_USUARIO'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'GET'.
        CONCATENATE lwa_webservice-url '/v1/usuario'   INTO lva_url.
      WHEN 'POST_USUARIO'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'POST'.
        CONCATENATE lwa_webservice-url '/v1/usuario'   INTO lva_url.
      WHEN 'PUT_USUARIO'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'PUT'.
        CONCATENATE lwa_webservice-url '/v1/usuario'   INTO lva_url.
      WHEN 'GET_USUARIOS'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'GET'.
        CONCATENATE lwa_webservice-url '/v1/usuarios'   INTO lva_url.
      WHEN 'GET_NOMES_ALE'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'GET'.
        CONCATENATE lwa_webservice-url '/v1/nomesAleatorios'   INTO lva_url.
      WHEN 'SET_ATIVAR'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'GET'.
        CONCATENATE lwa_webservice-url '/v1/Ativar'   INTO lva_url.
      WHEN 'SET_BLOQUEAR'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'POST'.
        CONCATENATE lwa_webservice-url '/v1/Bloquear'   INTO lva_url.
      WHEN 'GET_ESTRUT_ORG'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'GET'.
        CONCATENATE lwa_webservice-url '/v1/unidadeOrganizacional'   INTO lva_url.
      WHEN 'POST_ESTRUT_ORG'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'POST'.
        CONCATENATE lwa_webservice-url '/v1/unidadeOrganizacional'   INTO lva_url.
      WHEN 'PUT_ESTRUT_ORG'.
        me->zif_integracao_inject~at_info_request_http-ds_metodo =  'PUT'.
        CONCATENATE lwa_webservice-url '/v1/unidadeOrganizacional'   INTO lva_url.
    ENDCASE.

    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_url                = lva_url.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_info_request_http-ds_body = lwa_data-json.


    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Authorization' value = lwa_webservice-token ) TO me->zif_integracao_inject~at_header_fields.


  ENDMETHOD.
ENDCLASS.
