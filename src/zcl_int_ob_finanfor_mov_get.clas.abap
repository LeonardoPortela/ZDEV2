*&-------------------------------------------------------------------------------------------------------*
*& Report         : ZCL_INT_OB_FINANFOR_MOV_GET                                                          *
*& Chamado        : USER STORY 184694                                                                    *
*& Data           : 14/07/2025                                                                           *
*& Especificado   : Antonio Rodrigues                                                                    *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 14/07/2025  |DEVK9A2OOH  |NSEGATIN       |Integração Finanfor - Busca Movimento. Desenvolvim. inicial.*
*&                                          |Chamado: 184694.                                            *
*--------------------------------------------------------------------------------------------------------*
CLASS zcl_int_ob_finanfor_mov_get DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_outbound .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '301' ##NO_TEXT.
    CONSTANTS: at_servico      TYPE c LENGTH 20 VALUE 'FINANFOR_MOVIMENTO' ##NO_TEXT.                 "Integração MOV. Finanfor - Outbound

    DATA: at_param      TYPE zfie_finanfor_mov_get_param,
          at_tp_busca   TYPE char1 VALUE 'N' ##NO_TEXT,
          at_param_path TYPE char100.

    METHODS constructor
      IMPORTING
        VALUE(i_servico) TYPE ztipowebserv OPTIONAL
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_INT_OB_FINANFOR_MOV_GET IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ' '.

  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.
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

    TRY.
        CAST zcl_int_ob_token_finanfor( zcl_int_ob_token_finanfor=>zif_integracao_outbound~get_instance( )->execute_request( )
                                      )->zif_integracao_inject~get_header_request_http( IMPORTING e_header_fields = DATA(e_header_fields) ).

        APPEND LINES OF me->zif_integracao_inject~at_header_fields TO e_header_fields.

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 )
                             )
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

    DESCRIBE FIELD i_info_request TYPE DATA(vl_type).

    IF vl_type EQ 'v'. "Deep structure
      MOVE-CORRESPONDING i_info_request TO at_param.

    ELSE. "vl_type = g - String.
      CHECK i_info_request(1) EQ sy-abcde+12(1). "M - Busca Movimento de Pacote Negociado
      at_tp_busca   = i_info_request(1).
      at_param_path = i_info_request+1.

    ENDIF.

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
* Verifica se o tipo de busca é Pacote de Movimento Negociado.
    CHECK at_tp_busca EQ sy-abcde+13(1). "N - Busca Pacote de Movimento Negociado

    CLEAR: e_data.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = me->at_param
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      RECEIVING
        r_json      = e_data.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound = me.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_finanfor_mov_get.
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

    CONSTANTS: cl_content_type TYPE string VALUE 'Content-Type',
               cl_origin       TYPE string VALUE 'origin',       "Informado em todos os endpoints da API Finanfor
               cl_trades       TYPE string VALUE 'trades',       "Corresponde ao parâmetro "category" da URL
               cl_search       TYPE string VALUE 'search',
               cl_page         TYPE string VALUE 'page',         "Parâmetro Page
               cl_size         TYPE string VALUE 'size'.         "Parâmetro Size

    r_if_integracao_outbound = me.

    SELECT SINGLE * FROM zauth_webservice
       INTO @DATA(lwa_webservice)
    WHERE service EQ @at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = CONV #( at_servico ) )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = CONV #( at_servico ).
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
* sy-abcde+13(1) = "N - Busca Pacote de Movimento Negociado
* sy-abcde+12(1) = "M - Busca Movimento de Pacote Negociado
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = COND #( WHEN at_tp_busca EQ sy-abcde+13(1) THEN me->zif_integracao_inject~co_request_method_post
                                                                                   WHEN at_tp_busca EQ sy-abcde+12(1) THEN me->zif_integracao_inject~co_request_method_get ).
* Monta os dados dos Parâmetros da Busca do(s) Movimento(s).
    me->zif_integracao_inject~at_info_request_http-ds_url                = COND #( WHEN at_tp_busca EQ sy-abcde+13(1) THEN lwa_webservice-url && cl_trades && '/' && cl_search
                                                                                   WHEN at_tp_busca EQ sy-abcde+12(1) THEN lwa_webservice-url && cl_trades && '/' && at_param_path ).
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.
*** Informação do header
    APPEND VALUE #( name = cl_content_type value = lwa_webservice-content_type ) TO me->zif_integracao_inject~at_header_fields.
    DATA(vl_origin) = |{ lwa_webservice-add01 CASE = LOWER }|.
    APPEND VALUE #( name = cl_origin value = vl_origin ) TO me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.
ENDCLASS.
