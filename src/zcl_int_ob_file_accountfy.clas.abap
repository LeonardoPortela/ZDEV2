CLASS zcl_int_ob_file_accountfy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_outbound .

    TYPES:
      BEGIN OF ty_str,
        cnpj   TYPE stcd1,
        ano(6) TYPE c,
        str    TYPE string,
        file   TYPE string,
        filex  TYPE xstring,
        fsize  TYPE i,
        path   TYPE string,
      END OF ty_str .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '173' ##NO_TEXT.
    DATA at_file TYPE ty_str .

    METHODS constructor
      IMPORTING
        VALUE(i_servico) TYPE ztipowebserv OPTIONAL
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_OB_FILE_ACCOUNTFY IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.

*    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_sim.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'interface'.

    me->zif_integracao_outbound~set_url( ).

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

    TRY .
        CAST zcl_int_ob_token_accountfy(
               zcl_int_ob_token_accountfy=>zif_int_ob_token_accountfy~get_instance(
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
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields  = i_form_fields.
*    APPEND VALUE #( HEADER_FIELD = 'File'      HEADER_VALUE = |{ me->at_file-filex }| ) TO me->zif_integracao_inject~at_form_fields.
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

*    LOOP AT me->at_dados_funcionarios INTO DATA(lwa_dados_funcinarios).
*
*      UPDATE ZHCMT0007 SET int_sistemas_legado = abap_true
*                           dt_int_legado       = sy-datum
*                           hr_int_legado       = sy-uzeit
*       WHERE pernr EQ lwa_dados_funcinarios-pernr.
*
*    ENDLOOP.

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

*   DATA: LIT_ZHCMT0007 TYPE ZHCMT0007_T.

    r_if_integracao_outbound = me.

*   MOVE-CORRESPONDING i_info_request TO LIT_ZHCMT0007.

    me->at_file = i_info_request.

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

    DATA: lva_url       TYPE string,
          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'ACCOUNTFY_SEND_FILE'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'ACCOUNTFY_SEND_FILE' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'ACCOUNTFY_SEND_FILE'.
    ENDIF.

*    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

*    lva_url = |{ lwa_webservice-url }| && |{ 'datafile/' }| && |{ lwa_webservice-username(9) }| && |{ '/' }| && |{ me->at_file-cnpj }| && |{ '/' }| && |{ me->at_file-ano }| && |{ '/XLS' }| && |{ '/BALANCETE' }| && |{ '/upload' }|.


*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data   = me->at_file
*      RECEIVING
*        r_json = e_data.

*    e_data = |{ '{' }| && |{ me->at_file-str }| && |{ '}' }|.

    e_data = me->at_file-str.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound = me.
    "e_referencia-tp_referencia = 'ENV_FUNCIONARIOS_LEGADOS'.
    "e_referencia-id_referencia =

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_file_accountfy.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  ENDMETHOD.


  METHOD zif_integracao_outbound~send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_integracao_outbound = me.

    CREATE OBJECT lc_integrar.

*    DATA(lv_value) = 'form-data; name="documento"; filename="' && me->at_file-file && '"'.
*    APPEND VALUE #( header_field = 'content-disposition'      header_value = lv_value   xvalue = me->at_file-filex ) TO me->zif_integracao_inject~at_multipart_fields.


*    DATA(lv_value) = 'form-data; name="documento"; filename="' && me->at_file-file && '"'.
*    APPEND VALUE #( header_field = 'content-disposition'      header_value = lv_value   xvalue = me->at_file-filex ) TO lc_integrar->zif_integracao~at_multipart.
*    APPEND VALUE #( header_field = 'file' header_value = 'file' value = |{ me->at_file-path }| && |{ me->at_file-file }| ) TO lc_integrar->zif_integracao~at_multipart.

    DATA(lv_value) = 'form-data; name="file"; filename="' && |{ me->at_file-file }| && '"'.
    APPEND VALUE #( header_field = 'content-type' header_value = 'application/zip' ) TO me->zif_integracao_inject~at_multipart_fields.
    APPEND VALUE #( header_field = 'content-disposition'      header_value = lv_value   xvalue = me->at_file-filex ) TO lc_integrar->zif_integracao~at_multipart.


    lc_integrar->zif_integracao~at_msg_integra-ds_not_content_length = abap_true.

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

*    me->zif_integracao_inject~at_info_request_http-ds_body = i_data.
*    me->zif_integracao_inject~at_info_request_http-ds_form_data = i_data.
*    me->zif_integracao_inject~at_info_request_http-ds_body_xstring = me->at_file-filex.
*    me->zif_integracao_inject~at_info_request_http-ds_formato = 'FILE'.
*    me->zif_integracao_inject~at_info_request_http-ds_data_xstring = me->at_file-filex.

    me->zif_integracao_inject~at_info_request_http-ds_content_type = 'multipart/form-data'.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = 'HTTP/1.1'.

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_id_referencia.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~set_url.

    DATA: lva_url       TYPE string,
          lv_mediatype  TYPE string,
          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'ACCOUNTFY_SEND_FILE'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'ACCOUNTFY_SEND_FILE' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'ACCOUNTFY_SEND_FILE'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    IF me->at_file-cnpj IS INITIAL OR me->at_file-cnpj EQ 0.
      lv_mediatype = '/multibalancete'.
      CONDENSE me->at_file-ano NO-GAPS.
    ELSE.
      lv_mediatype = '/balancete'.
    ENDIF.

    lva_url = |{ lwa_webservice-url }| && |{ 'datafile/' }| && |{ lwa_webservice-username(9) }| && |{ '/' }| && |{ me->at_file-cnpj }| && |{ '/' }| && |{ me->at_file-ano }| && |{ '/CSV' }| && |{ lv_mediatype }| && |{ '/upload' }|.
    lva_url_token = lwa_webservice-url_token.

    CLEAR: me->zif_integracao_inject~at_header_fields.


*  APPEND VALUE #( name = 'Content-Type' value = 'multipart/form-data' ) TO me->zif_integracao_inject~at_header_fields.
*  APPEND VALUE #( name = '~request_method' value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
*  APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
*  APPEND VALUE #( name = 'Content-Length' value = '' ) TO me->zif_integracao_inject~at_header_fields.
*
*  me->zif_integracao_inject~at_header_fields = VALUE #(
*                            ( name = 'Accept' value = '*/*' )
*                            ( name = 'Accept-Encoding' value = 'gzip, deflate, br' )
*                            ( name = 'Content-Type' value = 'multipart/form-data' )
*                            ).

    me->zif_integracao_inject~at_info_request_http-ds_content_type     = 'multipart/form-data;charset=UTF-8'.
    me->zif_integracao_inject~at_info_request_http-ds_url              = lva_url.
    me->zif_integracao_inject~at_info_request_http-ds_url_token        = lva_url_token.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = 'HTTP/1.1'.
    me->zif_integracao_inject~at_info_request_http-ds_metodo           = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

    DATA(lv_value) = 'form-data; name="file"; filename="' && |{ me->at_file-file }| && '"'.
    APPEND VALUE #( header_field = 'content-type' header_value = 'application/zip' ) TO me->zif_integracao_inject~at_multipart_fields.
    APPEND VALUE #( header_field = 'content-disposition'      header_value = lv_value   xvalue = me->at_file-filex ) TO me->zif_integracao_inject~at_multipart_fields.

  ENDMETHOD.
ENDCLASS.
