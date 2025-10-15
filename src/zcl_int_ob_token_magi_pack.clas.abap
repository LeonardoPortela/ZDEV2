CLASS zcl_int_ob_token_magi_pack DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_outbound .

    TYPES:
      BEGIN OF ty_token,
        tipo TYPE char01,
      END OF ty_token .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '241' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA at_param_webservice TYPE zauth_webservice .
ENDCLASS.



CLASS ZCL_INT_OB_TOKEN_MAGI_PACK IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.
    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.
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

  ENDMETHOD.


  METHOD zif_integracao_inject~set_form_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields = i_form_fields.
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
    r_if_integracao_inject = me.
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
  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

    r_if_integracao_outbound = me.

    DATA(lit_header_auth) = zcl_gestao_token=>get_token_valido( i_id_token = '0005' ).

    IF lit_header_auth[] IS NOT INITIAL.
      me->zif_integracao_inject~at_header_fields = lit_header_auth.
      EXIT.
    ENDIF.


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

    SELECT SINGLE *
      FROM zauth_webservice INTO me->zif_integracao_outbound~at_auth_webservice
     WHERE service = 'MAGIPACK_TOKEN'.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'MAGIPACK_TOKEN' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'MAGIPACK_TOKEN'.
    ENDIF.

    TRANSLATE me->zif_integracao_outbound~at_auth_webservice-add01 TO LOWER CASE.

    APPEND VALUE #( name = 'client_id'       value = me->zif_integracao_outbound~at_auth_webservice-username ) TO me->zif_integracao_inject~at_form_fields.
    APPEND VALUE #( name = 'client_secret'   value = me->zif_integracao_outbound~at_auth_webservice-password ) TO me->zif_integracao_inject~at_form_fields.
    APPEND VALUE #( name = 'scope'           value = me->zif_integracao_outbound~at_auth_webservice-add01 ) TO me->zif_integracao_inject~at_form_fields.
    APPEND VALUE #( name = 'grant_type'      value = me->zif_integracao_outbound~at_auth_webservice-add02 ) TO me->zif_integracao_inject~at_form_fields.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound = me.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_token_magi_pack.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  METHOD zif_integracao_outbound~send_msg.

    TYPES: BEGIN OF ty_oauth,
             token_type     TYPE string,
             expires_in     TYPE string,
             ext_expires_in TYPE string,
             access_token   TYPE string,
           END OF ty_oauth .

    DATA: lva_token TYPE string.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_oauth.

    r_if_integracao_outbound = me.

    CREATE OBJECT lc_integrar.

*    FREE: me->zif_integracao_inject~AT_FORM_FIELDS.
    APPEND VALUE #( name = 'client_id'       value = me->zif_integracao_outbound~at_auth_webservice-username ) TO lc_integrar->zif_integracao~at_form_fields.
    APPEND VALUE #( name = 'client_secret'   value = me->zif_integracao_outbound~at_auth_webservice-password ) TO lc_integrar->zif_integracao~at_form_fields.
    APPEND VALUE #( name = 'scope'           value = me->zif_integracao_outbound~at_auth_webservice-add01 ) TO lc_integrar->zif_integracao~at_form_fields.
    APPEND VALUE #( name = 'grant_type'      value = me->zif_integracao_outbound~at_auth_webservice-add02 ) TO lc_integrar->zif_integracao~at_form_fields.

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
    FREE: lc_integrar.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

    CLEAR: me->zif_integracao_inject~at_header_fields.

    lva_token = |Bearer { lc_retorno-access_token } |.

    APPEND VALUE #( name = 'Authorization'  value = lva_token ) TO me->zif_integracao_inject~at_header_fields.

    zcl_gestao_token=>update_token( i_id_token = '0005' i_access_token = lc_retorno-access_token  i_token_type = 'Bearer' ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_data.

    r_if_integracao_outbound = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_data.
    me->zif_integracao_inject~at_info_request_http-ds_form_data = i_data.

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

    CLEAR: me->zif_integracao_inject~at_header_fields.
*    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_url                = me->zif_integracao_outbound~at_auth_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = me->zif_integracao_inject~co_request_method_post.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = 'application/x-www-form-urlencoded'. "'application/json'.

    FREE: me->zif_integracao_inject~at_header_fields. "me->zif_integracao_inject~at_multipart_fields.
    APPEND VALUE #( name = '~request_method'     value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'   value = 'application/x-www-form-urlencoded' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
