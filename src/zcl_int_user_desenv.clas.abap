CLASS zcl_int_user_desenv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_int_user_desenv .

    TYPES:
      BEGIN OF ty_saida,
      code           TYPE     char10, "pa0000-pernr,
      name           TYPE     pa0002-cname,
      login          TYPE     pa0465-cpf_nr,
      password       TYPE     pa0465-cpf_nr,
      status         TYPE     char1,
      email          TYPE     string,
      phone          TYPE     char1,
      positioncode   TYPE     char10,
      locationcode   TYPE     pa0001-kostl,
      stationcode    TYPE     char1,
      stationname    TYPE     char1,
      teamcode       TYPE     char10, "pa0001-orgeh,
      teamname       TYPE     hrp1000-stext,
      teamleadercode TYPE     char10, "pa9002-pernimed,
*      profilecode    TYPE     char4,
      END OF ty_saida .
    TYPES:
      tyt_saida TYPE STANDARD TABLE OF ty_saida .


    TYPES: BEGIN OF ty_msg,
             type    TYPE char1,    "type": "I – informação, A – alerta, E - erro",
             message TYPE char20, "message": "Descrição da mensagem"
           END OF ty_msg,
           tyt_msg TYPE STANDARD TABLE OF ty_msg.

*    TYPES: BEGIN OF ty_returnmessages,
*             receivedobject TYPE ty_saida, "tipo tabela
*             messages       TYPE tyt_msg, "tipo de tabela
*             imported       TYPE char5,
*           END OF ty_returnmessages,
*           tyt_returnmessages TYPE STANDARD TABLE OF ty_returnmessages.


    TYPES: BEGIN OF ty_struct,      ":0,"importedItems":0,"returnMessages":[]
             receiveditems(3) TYPE n,
             importeditems(3) TYPE n,
*             returnmessages   TYPE tyt_returnmessages,
           END OF ty_struct.



    DATA gt_dados TYPE tyt_saida .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_USER_DESENV IMPLEMENTATION.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_true.
  endmethod.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.

    r_if_integracao_inject = me.
    TRY .
        CAST zcl_int_token_desenv(
               zcl_int_token_desenv=>zif_int_token_desenv~get_instance(
                 )->get_token( )
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

*    APPEND VALUE #( name = 'Accept' value = 'application/xml' ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.

    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.

  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  endmethod.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    e_sucesso = abap_true.
  endmethod.


  METHOD zif_int_user_desenv~enviar_desenvolve.

    DATA lv_text2 TYPE char100.
    DATA lo_xml_ret TYPE REF TO cl_xml_document.

    CHECK it_saida[] IS NOT INITIAL.
    gt_dados[] = it_saida[].

    me->zif_int_user_desenv~set_servico( i_servico = me->zif_int_user_desenv~gc_service ).
    me->zif_int_user_desenv~set_id_referencia( ).
    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_desenv_user.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_int_user_desenv~at_struct              = it_saida.

    LOOP AT me->zif_int_user_desenv~at_struct ASSIGNING FIELD-SYMBOL(<f_saida>).
      REPLACE ALL OCCURRENCES OF '.' IN <f_saida>-login WITH ''.
      REPLACE ALL OCCURRENCES OF '-' IN <f_saida>-login WITH ''.

      REPLACE ALL OCCURRENCES OF '.' IN <f_saida>-password WITH ''.
      REPLACE ALL OCCURRENCES OF '-' IN <f_saida>-password WITH ''.
    ENDLOOP.


    SELECT SINGLE *
      FROM zauth_webservice
      INTO me->zif_int_user_desenv~at_auth_ws
        WHERE service = me->zif_int_user_desenv~gc_service.

    SELECT SINGLE *
      FROM zauth_webservice
      INTO me->zif_int_user_desenv~at_token_ws
        WHERE service = me->zif_int_user_desenv~gc_token.

    IF me->zif_int_user_desenv~at_auth_ws  IS INITIAL OR
       me->zif_int_user_desenv~at_token_ws IS INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao.
    ENDIF.

    CREATE OBJECT lo_xml_ret.

    CLEAR r_return_value.

    me->zif_int_user_desenv~set_ds_url(
      )->set_ds_data(
      )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao) e_integracao  = DATA(e_integracao) ).

    DATA: ls_struct TYPE ty_struct.

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = ls_struct ).

    IF ls_struct-importeditems LE 0.
      ev_return_code = ls_struct-importeditems.
      ev_return_msg = 'ERROR'.
      SHIFT ev_return_msg LEFT DELETING LEADING space.
      zcx_error=>zif_error~gera_erro_geral( i_texto = ev_return_msg ).
    ELSE.
      ev_return_code = ls_struct-importeditems.
      FREE: ev_return_msg.
    ENDIF.
  ENDMETHOD.


  METHOD zif_int_user_desenv~get_id_referencia.
    r_if_int_user_desenv = me.
    e_referencia-tp_referencia = me->zif_int_user_desenv~gc_service.

    READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>) INDEX 1.
    IF sy-subrc IS INITIAL AND <fs_dados> IS ASSIGNED AND <fs_dados>-code IS NOT INITIAL.
      e_referencia-id_referencia = <fs_dados>-code.
    ELSE.
      e_referencia-id_referencia = sy-datum.
    ENDIF.
  ENDMETHOD.


  method ZIF_INT_USER_DESENV~SET_DS_DATA.
     me->zif_integracao_inject~at_info_request_http-ds_body = /ui2/cl_json=>serialize( data = me->zif_int_user_desenv~at_struct ).
     me->zif_int_user_desenv~at_body = me->zif_integracao_inject~at_info_request_http-ds_body.
     r_if_int_user_desenv = me.
  endmethod.


  METHOD zif_int_user_desenv~set_ds_url.
    me->zif_integracao_inject~at_info_request_http-ds_formato      = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type = me->zif_int_user_desenv~at_auth_ws-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url = me->zif_int_user_desenv~at_auth_ws-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo = me->zif_int_user_desenv~at_auth_ws-method.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo = ''.
    me->zif_int_user_desenv~set_id_referencia( ).
    r_if_int_user_desenv = me.
  ENDMETHOD.


  METHOD zif_int_user_desenv~set_id_referencia.

    r_if_int_user_desenv = me.
    me->zif_int_user_desenv~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).
  ENDMETHOD.


  METHOD zif_int_user_desenv~set_send_msg.

    DATA lc_integrar TYPE REF TO zcl_integracao.

    r_if_int_user_desenv = me.

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


  method ZIF_INT_USER_DESENV~SET_SERVICO.

    IF i_servico IS NOT INITIAL.
      zif_int_user_desenv~at_servico = i_servico.
    ENDIF.

  endmethod.
ENDCLASS.
