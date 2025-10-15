class ZCL_INT_CLIENTE_TOKEN definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INT_CLIENTE_TOKEN .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_CLIENTE_TOKEN IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interface_cliente_token.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_form_fields = me->zif_integracao_inject~at_form_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_form_fields = i_form_fields.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  METHOD zif_int_cliente_token~get_instance.

    IF zif_int_cliente_token~at_if_int_cliente_token IS NOT BOUND.
      CREATE OBJECT zif_int_cliente_token~at_if_int_cliente_token
        TYPE zcl_int_cliente_token.
    ENDIF.

    r_if_int_cliente_token = zif_int_cliente_token~at_if_int_cliente_token.
  ENDMETHOD.


  METHOD zif_int_cliente_token~get_token.
    DATA: l_xml               TYPE string,
          l_table_timestamp   TYPE p,
          l_current_timestamp TYPE p.

    DATA: lt_zintegracao0002 TYPE STANDARD TABLE OF zintegracao0002,
          lt_tvarvc          TYPE STANDARD TABLE OF tvarvc.

    DATA: const_grant_type         VALUE 'grant_type',
          const_client_credentials VALUE 'client_credentials'.

    r_if_int_cliente_token = me.


*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Ini
*--------------------------------------------------------------------------------------------------------------------*
    SELECT SINGLE *
      FROM zintegracao0002 INTO @DATA(ls_zintegracao0002)
      WHERE id_token EQ '0001'.

    IF sy-subrc IS INITIAL.

      l_table_timestamp   = ls_zintegracao0002-time_stamp.

      GET TIME STAMP FIELD DATA(l_timestamp_before).
      l_current_timestamp = l_timestamp_before.

      IF cl_abap_tstmp=>subtract( tstmp1 = l_current_timestamp
                                  tstmp2 = l_table_timestamp ) < ls_zintegracao0002-expires_in.

        "Token ainda está válido.
        CONCATENATE ls_zintegracao0002-token_type ls_zintegracao0002-access_token INTO DATA(l_header_token) SEPARATED BY space.
        CLEAR: me->zif_integracao_inject~at_header_fields.

        me->zif_integracao_inject~at_header_fields =
        VALUE #(
                  ( name  = 'Authorization'   value = l_header_token )
               ).

        EXIT.
      ENDIF.

    ENDIF.

*--------------------------------------------------------------------------------------------------------------------*
*  Verificação Existencia Token Valido - Fim
*--------------------------------------------------------------------------------------------------------------------*

    FREE: l_xml.

    SELECT SINGLE * INTO @DATA(zauth_webservice)
      FROM zauth_webservice
     WHERE service    EQ 'AMAGGION_TOKEN'.


    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_int_cliente_token~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_int_cliente_token~at_servico ).
    ENDIF.



    CLEAR: me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
    APPEND VALUE #( name = 'Content-Type'     value = 'application/x-www-form-urlencoded' ) TO me->zif_integracao_inject~at_header_fields.

*    CLEAR: me->zif_integracao_inject~at_form_fields.
*    APPEND VALUE #( name = zauth_webservice-username  value = zauth_webservice-password ) TO me->zif_integracao_inject~at_form_fields.
*    APPEND VALUE #( name = 'client_id' value = 'amaggienergia' ) TO me->zif_integracao_inject~at_form_fields.
*    APPEND VALUE #( name = 'grant_type'               value = 'client_credentials' ) TO me->zif_integracao_inject~at_form_fields.

*    me->zif_integracao_inject~at_info_request_http-ds_body = l_xml.

    TRY.
        me->zif_int_cliente_token~set_ds_url( ).
        me->zif_int_cliente_token~set_send_msg( IMPORTING
                                                      e_access_token  = DATA(e_access_token)
                                                      e_token_type    = DATA(e_token_type)
                                                      e_expires_in    = DATA(e_expires_in)
                                              ).


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

    GET TIME STAMP FIELD DATA(l_timestamp_after).

    APPEND VALUE #( id_token     = '0001'
                    access_token = e_access_token
                    token_type   = e_token_type
                    expires_in   = e_expires_in - 300
                    time_stamp   = l_timestamp_after
                  ) TO lt_zintegracao0002.

    MODIFY zintegracao0002 FROM TABLE lt_zintegracao0002.
  ENDMETHOD.


  METHOD zif_int_cliente_token~set_ds_url.
    DATA: const_grant_type         VALUE 'grant_type',
          const_client_credentials VALUE 'client_credentials'.

    r_if_int_cliente_token = me.

    SELECT SINGLE * INTO @DATA(zauth_webservice)
      FROM zauth_webservice
     WHERE service    EQ 'AMAGGION_TOKEN'.


    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_int_cliente_token~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_int_cliente_token~at_servico ).
    ENDIF.

    "Montagem do json como autenticação.
    DATA(json) = '{ "username":' && '"paulo", ' && '"password":' && '" 4oC1C5p7RcLBtAiZqpKS" }'.


*    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
*    me->zif_integracao_inject~at_info_request_http-ds_content_type = zauth_webservice-content_type.
*    me->zif_integracao_inject~at_info_request_http-ds_url_token    = zauth_webservice-url_token.

    me->zif_integracao_inject~at_info_request_http-ds_url = zauth_webservice-url_token.

    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'POST'.
*    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
*    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


  ENDMETHOD.


  METHOD zif_int_cliente_token~set_send_msg.
    TYPES: BEGIN OF ty_retorno,
             access_token TYPE string,
             token_type   TYPE string,
             expires_in   TYPE string,
           END OF ty_retorno.

    DATA: lc_integrar TYPE REF TO zcl_integracao,
          lc_retorno  TYPE ty_retorno.

    r_if_int_cliente_token = me.

    CREATE OBJECT lc_integrar.


*    me->zif_integracao_inject~at_form_fields
    TRY.
        me->zif_integracao_inject~at_info_request_http-ds_content_type = 'application/x-www-form-urlencoded'.

        lc_integrar->zif_integracao~at_form_fields = me->zif_integracao_inject~at_form_fields.

        "Cria MSG para Integração via HTTP
        lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
          )->set_new_msg( IMPORTING e_id_integracao = DATA(e_id_integracao)
          )->set_outbound_msg(
          )->set_processar_retorno(
          )->set_integrar_retorno(
          )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
          )->free(
          ).

        FREE: lc_integrar.
        CLEAR: lc_integrar.

        /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = lc_retorno ).

        e_access_token = lc_retorno-access_token.
        e_token_type   = lc_retorno-token_type.
        e_expires_in   = lc_retorno-expires_in.

        CONCATENATE e_token_type e_access_token INTO DATA(l_header_token) SEPARATED BY space.
        CLEAR: me->zif_integracao_inject~at_header_fields.
        APPEND VALUE #( name = 'Authorization'  value = l_header_token ) TO me->zif_integracao_inject~at_header_fields.


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
ENDCLASS.
