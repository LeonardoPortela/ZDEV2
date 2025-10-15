class ZCL_INT_OB_SEND_ANEXO_REQ definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '225' ##NO_TEXT.
  data AT_ANEXO type ZDE_ZMME0001 .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_INT_OB_SEND_ANEXO_REQ IMPLEMENTATION.


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

    TRY .
        CAST zcl_integracao_token_coupa(
                zcl_integracao_token_coupa=>zif_integracao_token_coupa~get_instance(
                  )->get_token( )
              )->zif_integracao_inject~get_header_request_http(
           IMPORTING
             e_header_fields = DATA(e_header_fields) ).

        APPEND VALUE #( name = 'accept'   value = 'application/json' ) TO e_header_fields.
        APPEND VALUE #( name = 'Content-Type'   value = 'multipart/form-data' ) TO e_header_fields.

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


  method zif_integracao_outbound~build_info_request.

    data: wa_anexo type zde_zmme0001.

    r_if_integracao_outbound = me.

    move-corresponding i_info_request to wa_anexo.

    me->at_anexo = wa_anexo.

  endmethod.


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


  method ZIF_INTEGRACAO_OUTBOUND~GET_DATA.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

*    CALL METHOD /ui2/cl_json=>serialize
*       EXPORTING
*         data   = me->at_dados_ordem_orc
*       RECEIVING
*         r_json = e_data.


  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    "e_referencia-tp_referencia = 'ENV_FUNCIONARIOS_LEGADOS'.
    "e_referencia-id_referencia =

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_send_anexo_req.
    ENDIF.

    r_if_integracao_outbound = zif_integracao_outbound~at_if_integracao_outbound.

  endmethod.


  method zif_integracao_outbound~send_msg.

    data: lc_integrar type ref to zcl_integracao.

    r_if_integracao_outbound = me.

    create object lc_integrar.

    data(lv_value) = |form-data; name="attachment[file]"; filename="{ me->at_anexo-descript }.{ me->at_anexo-docuclass }"|.
    append value #( header_field = 'Content-Type' header_value = |application/{ me->at_anexo-docuclass }|  ) to lc_integrar->zif_integracao~at_multipart.
    case me->at_anexo-docuclass+0(3).
      when 'xml' or 'txt'.
        append value #( header_field = 'content-disposition' header_value = lv_value xvalue = me->at_anexo-doc_string ) to lc_integrar->zif_integracao~at_multipart.
      when 'pdf' or 'xls' or 'xlsx' or 'docx' or 'doc'.
        append value #( header_field = 'content-disposition' header_value = lv_value xvalue = me->at_anexo-doc_xstring ) to lc_integrar->zif_integracao~at_multipart.
      when others.
        append value #( header_field = 'content-disposition' header_value = lv_value xvalue = me->at_anexo-doc_string ) to lc_integrar->zif_integracao~at_multipart.
    endcase.


    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = cast #( me )
      )->set_new_msg( importing e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( importing e_integracao = e_integracao
      )->free(
      ).

    clear: lc_integrar.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_DATA.

    r_if_integracao_outbound = ME.

    ME->zif_integracao_inject~at_info_request_http-ds_body = i_data.

  endmethod.


  method ZIF_INTEGRACAO_OUTBOUND~SET_ID_REFERENCIA.

    r_if_integracao_outbound = me.
    me->zif_integracao_outbound~get_id_referencia( IMPORTING e_referencia = me->zif_integracao_inject~at_referencia ).

  endmethod.


  method zif_integracao_outbound~set_url.

    data: lva_url       type string,
          lva_url_token type string.

    r_if_integracao_outbound = me.

    select single *
      from zauth_webservice into @data(lwa_webservice)
     where service = 'SEND_ANEXO_REQ_COMPRA_COUPA'.

    if sy-subrc is not initial.
      raise exception type zcx_integracao
        exporting
          textid = value #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'SEND_ANEXO_REQ_COMPRA_COUPA' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'SEND_ANEXO_REQ_COMPRA_COUPA'.
    endif.

    free: me->zif_integracao_inject~at_header_fields, me->zif_integracao_inject~at_multipart_fields.
    append value #( name = 'accept'   value = |application/{ me->at_anexo-docuclass }| ) to me->zif_integracao_inject~at_header_fields.
    append value #( name = 'Content-Type'   value = 'multipart/form-data' ) to me->zif_integracao_inject~at_header_fields.
*
**    append value #( value = 'attachment[file]'   xvalue = me->at_anexo ) to me->zif_integracao_inject~at_multipart_fields.
**    append value #( value = 'attachment[type]'   xvalue = 'file'       ) to me->zif_integracao_inject~at_multipart_fields.
*
*    data(lv_value) = 'form-data; name="documento"; filename="' && 'attachment[file]' && '"'.
*    append value #( header_field = 'Content-Type' header_value = 'multipart/form-data'  ) to me->zif_integracao~at_multipart.
*    append value #( header_field = 'content-disposition' header_value = lv_value xvalue = me->at_anexo ) to me->zif_integracao~at_multipart.
*    me->zif_integracao~at_multipart


*    if_rest_message=>gc_method_post = 'GET'.
    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = 'multipart/form-data;charset=UTF-8'.
    me->zif_integracao_inject~at_info_request_http-ds_url                = lwa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = 'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  endmethod.
ENDCLASS.
