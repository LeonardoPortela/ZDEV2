CLASS zcl_int_ob_send_nfe_ent_luft DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_outbound .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '285' ##NO_TEXT.
    DATA at_dados_nfe TYPE zsde_nfe .

    METHODS constructor
      IMPORTING
        VALUE(i_servico) TYPE ztipowebserv OPTIONAL
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_OB_SEND_NFE_ENT_LUFT IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_sim.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'LUFT'.

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

    CHECK me->zif_integracao_inject~at_header_fields IS INITIAL.

    TRY.
        CAST zcl_int_ob_token_cd_luft( zcl_int_ob_token_cd_luft=>zif_integracao_outbound~get_instance( )->execute_request( )
                                      )->zif_integracao_inject~get_header_request_http( IMPORTING e_header_fields = DATA(e_header_fields) ).

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


  METHOD zif_integracao_outbound~execute_request.

    DATA: wa_nfe TYPE zsde_nfe.

    r_if_integracao_outbound = me.

    MOVE i_info_request TO wa_nfe-chave_acesso.

    SELECT SINGLE branch , forne_cnpj, forne_cpf,dt_emissao
      FROM zib_nfe_dist_ter INTO @DATA(lwa_zib_nfe_dist_ter)
     WHERE chave_nfe = @wa_nfe-chave_acesso.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE chave_nfe, prod_ncm
     FROM zib_nfe_dist_itm INTO @DATA(lwa_zib_nfe_dist_itm)
    WHERE chave_nfe = @wa_nfe-chave_acesso.

    IF lwa_zib_nfe_dist_ter-forne_cnpj IS NOT INITIAL.

      SELECT SINGLE *
        FROM zmmt0214 INTO @DATA(lwa_zmmt0214)
       WHERE cnpj EQ @lwa_zib_nfe_dist_ter-forne_cnpj.

    ELSEIF lwa_zib_nfe_dist_ter-forne_cpf IS NOT INITIAL.

      SELECT SINGLE *
        FROM zmmt0214 INTO @lwa_zmmt0214
       WHERE cpf EQ @lwa_zib_nfe_dist_ter-forne_cpf.

    ELSE.
       sy-subrc = 4.
    ENDIF.

    IF zcl_util_sd=>ck_integration_luft_active( i_direcao = 'E' ) EQ abap_false.
      RETURN.
    ENDIF.

    CHECK ( sy-subrc EQ 0  ) AND
          ( lwa_zmmt0214-periodo_ini IS NOT INITIAL AND lwa_zmmt0214-periodo_fim IS NOT INITIAL ) AND
          ( lwa_zmmt0214-periodo_ini <= lwa_zib_nfe_dist_ter-dt_emissao ) AND
          ( lwa_zmmt0214-periodo_fim >= lwa_zib_nfe_dist_ter-dt_emissao ).

    SELECT SINGLE *
      FROM zmmt0215 INTO @DATA(lwa_zmmt0215)
     WHERE branch EQ @lwa_zib_nfe_dist_ter-branch.

    CHECK ( sy-subrc EQ 0  ) AND lwa_zib_nfe_dist_ter-branch IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0216 INTO @DATA(lwa_zmmt0216)
     WHERE steuc EQ @lwa_zib_nfe_dist_itm-prod_ncm.

    CHECK ( sy-subrc EQ 0  ) AND lwa_zib_nfe_dist_itm-prod_ncm IS NOT INITIAL.

    "Inclui Json na Mesagem a Ser Enviada
    me->zif_integracao_outbound~build_info_request( i_info_request = i_info_request
      )->get_data( IMPORTING e_data = DATA(lc_data)
      )->set_data( EXPORTING i_data = lc_data
      )->set_url(
      )->set_id_referencia(
      )->send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao	= e_integracao
      ).

  ENDMETHOD.


  METHOD zif_integracao_outbound~build_info_request.

    DATA: wa_nfe TYPE zsde_nfe.

    r_if_integracao_outbound = me.

    MOVE i_info_request TO wa_nfe-chave_acesso.

    DATA: lo_dom        TYPE REF TO if_ixml_document.
    DATA: out_binxml TYPE xstring.
    DATA: w_size     TYPE i.
    DATA: t_xml      TYPE dcxmllines.

    zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = CONV #( wa_nfe-chave_acesso )
                                                           i_direcao = CONV #( 'IN' )
                                                 IMPORTING e_xml_raw = DATA(o_xml_content) ).

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml           = o_xml_content
      IMPORTING
        document      = lo_dom
      EXCEPTIONS
        invalid_input = 1
        OTHERS        = 2.

    IF sy-subrc = 0.

      " Convert DOM to XML doc (table)

      CALL FUNCTION 'SDIXML_DOM_TO_XML'
        EXPORTING
          document      = lo_dom
          pretty_print  = ' '
        IMPORTING
          xml_as_string = out_binxml
          size          = w_size
        TABLES
          xml_as_table  = t_xml
        EXCEPTIONS
          no_document   = 1
          OTHERS        = 2.

      IF sy-subrc = 0 .

        wa_nfe-xml_conteudo_nf = zcl_string=>xstring_to_base64( out_binxml ).
        me->at_dados_nfe = wa_nfe.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_data.

    r_if_integracao_outbound = me.

    CLEAR: e_data.

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        =  me->at_dados_nfe
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      RECEIVING
        r_json = e_data.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound = me.
    e_referencia-tp_referencia = 'CHAVE_NFE_ENT_LUFT'.
    e_referencia-id_referencia = me->at_dados_nfe-chave_acesso.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_send_nfe_ent_luft.
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

    DATA: lva_val       TYPE c,
          lva_url       TYPE string,
          lva_url_token TYPE string.

    r_if_integracao_outbound = me.

    SELECT SINGLE *
      FROM zauth_webservice INTO @DATA(lwa_webservice)
     WHERE service = 'LUFT_INTEGRACAO_NFE_ENTRADA'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'LUFT_INTEGRACAO_NFE_ENTRADA' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'LUFT_INTEGRACAO_NFE_ENTRADA'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    CLEAR: me->zif_integracao_inject~at_header_fields.
    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = lwa_webservice-method.
    me->zif_integracao_inject~at_info_request_http-ds_url_token          = lva_url_token.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_url                = lwa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


  ENDMETHOD.
ENDCLASS.
