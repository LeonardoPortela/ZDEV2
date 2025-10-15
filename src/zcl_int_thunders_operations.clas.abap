CLASS zcl_int_thunders_operations DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_int_thunders_operations .

    METHODS constructor
      IMPORTING
        !i_servico TYPE ztipowebserv
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_THUNDERS_OPERATIONS IMPLEMENTATION.


  METHOD constructor.
    me->zif_int_thunders_operations~set_servico( i_servico =  i_servico ).
    me->zif_integracao_inject~at_id_interface    = zif_integracao=>at_id_interf_thunders_operat.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_sim.

*    me->zif_int_thunders_operations~set_ds_url( ).

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
        CAST zcl_int_thunders_token(
               zcl_int_thunders_token=>zif_int_thunders_token~get_instance(
                 )->get_token( )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

*        IF me->zif_int_thunders_operations~at_query IS NOT INITIAL.
*          LOOP AT me->zif_int_thunders_operations~at_query ASSIGNING FIELD-SYMBOL(<ls_at_query>).
*            APPEND VALUE #( name = <ls_at_query>-name  value = <ls_at_query>-value ) TO e_header_fields.
*          ENDLOOP.
*        ENDIF.

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


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~get_dados_thunders_operations.

    r_if_int_thunders_operations = me.

    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_SENHA = I_SENHA.
    "ME->ZIF_INTEGRACAO_CLI_KUHLMANN~AT_USUARIO = I_USUARIO.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_int_thunders_operations~get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_ds_url(
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

    CASE me->zif_int_thunders_operations~at_service.
      WHEN 'THUNDERS'.
        /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = i_thunders_operations ).
      WHEN 'THUNDERS_CONCILIATIONS'.
        /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = i_thunders ).
      WHEN OTHERS.
    ENDCASE.



  ENDMETHOD.


  METHOD zif_int_thunders_operations~get_id_referencia.

    r_if_int_thunders_operations = me.

    e_referencia-tp_referencia = 'THUNDERS_OPERATIONS'.
    e_referencia-id_referencia = me->zif_int_thunders_operations~at_usuario.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~get_instance.

    IF zif_int_thunders_operations~at_if_int_thunders_operations IS NOT BOUND.
      CREATE OBJECT zif_int_thunders_operations~at_if_int_thunders_operations
        TYPE zcl_int_thunders_operations
        EXPORTING
          i_servico = i_servico. " Tipo de Serviço WebService
*        CATCH zcx_integracao.    " .
    ENDIF.

    r_if_int_thunders_operations = zif_int_thunders_operations~at_if_int_thunders_operations.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~get_json.
    DATA: lines TYPE char01.
    r_if_int_thunders_operations = me.
*    clear: lines.
*    IF me->zif_int_thunders_operations~at_query IS NOT INITIAL.
*      DESCRIBE TABLE me->zif_int_thunders_operations~at_query LINES lines.
*      e_json = '{' .
*
*      LOOP AT me->zif_int_thunders_operations~at_query ASSIGNING FIELD-SYMBOL(<ls_at_query>).
*        IF sy-tabix ne lines.
*         e_json =  e_json && '"' && <ls_at_query>-name && '"' && ':' && '"' && <ls_at_query>-value && '"' && ',' .
*        ELSE.
*          e_json =  e_json && '"' && <ls_at_query>-name && '"' && ':' && '"' && <ls_at_query>-value && '"'.
*        ENDIF.
*      ENDLOOP.
*
*      e_json = e_json && '}'.
*    ENDIF.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_ds_data.

    r_if_int_thunders_operations = me.

    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_ds_url.
    DATA: vg_param TYPE string,
          lines    TYPE char02.
    r_if_int_thunders_operations = me.

    SELECT SINGLE * INTO @DATA(zauth_webservice)
      FROM zauth_webservice
     WHERE service    EQ @me->zif_int_thunders_operations~at_service. "'THUNDERS'.


    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'K'
                            attr2 = me->zif_int_thunders_operations~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_int_thunders_operations~at_servico ).
    ENDIF.



    IF me->zif_int_thunders_operations~at_query IS NOT INITIAL.

      DESCRIBE TABLE me->zif_int_thunders_operations~at_query LINES lines.
      vg_param = '?' .

      LOOP AT me->zif_int_thunders_operations~at_query ASSIGNING FIELD-SYMBOL(<ls_at_query>).
        IF sy-tabix NE lines.
          vg_param =  vg_param && <ls_at_query>-name && '=' && <ls_at_query>-value && '&'.
        ELSE.
          vg_param =  vg_param && <ls_at_query>-name && '=' && <ls_at_query>-value.
        ENDIF.
      ENDLOOP.
    ENDIF.


    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = zauth_webservice-content_type.
*    me->zif_integracao_inject~at_info_request_http-ds_url_token    = zauth_webservice-url_token.
    me->zif_integracao_inject~at_info_request_http-ds_url = zauth_webservice-url && vg_param.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = 'GET'.
*    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
*    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    CLEAR: zauth_webservice.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_id_referencia.

    r_if_int_thunders_operations = me.
    me->zif_int_thunders_operations~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_json.

    r_if_int_thunders_operations = me.

    IF e_json IS NOT INITIAL.
      me->zif_int_thunders_operations~at_json = e_json.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_query.

    r_if_int_thunders_operations = me.

    IF i_parametro IS NOT INITIAL.
      me->zif_int_thunders_operations~at_query = i_parametro.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_int_thunders_operations = me.


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


  METHOD zif_int_thunders_operations~set_service.

    r_if_int_thunders_operations = me.

    CLEAR: me->zif_int_thunders_operations~at_service.

    IF i_service IS NOT INITIAL.
      me->zif_int_thunders_operations~at_service = i_service.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_servico.

    IF i_servico IS NOT INITIAL.
      zif_int_thunders_operations~at_servico = i_servico.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_thunders_operations~set_tipo.
    IF i_tipo IS NOT INITIAL.
      zif_int_thunders_operations~at_tipo = i_tipo.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
