class ZCL_INT_OB_SEND_NFE_SAI_LUFT definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_OUTBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '287' ##NO_TEXT.
  data AT_DADOS type ZSDS_NFE_SAI_LUFT .
  data AT_CHAVE_NFE type ZDE_CHAVE_DOC_E .

  methods CONSTRUCTOR
    importing
      value(I_SERVICO) type ZTIPOWEBSERV optional
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_OB_SEND_NFE_SAI_LUFT IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'OPUS'.

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


  METHOD zif_integracao_outbound~build_info_request.

    DATA: lv_docnum  TYPE j_1bdocnum,
          lv_out     TYPE xstring,
          lv_out_pdf TYPE xstring,
          lv_error   TYPE boolean,
          lv_key     TYPE j_1b_nfe_access_key,
          lv_chave   TYPE string,
          lva_lifnr  TYPE lfa1-lifnr.

    DATA: ls_nfe_saida TYPE zsds_nfe_sai_luft,
          ls_act       TYPE j_1bnfe_active.

    DATA: t_pdf_files TYPE zsdt_pdf_files.

    r_if_integracao_outbound = me.

    lv_docnum = i_info_request.

    CLEAR: lv_error, lv_chave, lv_key, lv_out, at_dados.

    DATA(lwa_romaneio) = zcl_les_utils=>get_romaneio_documento_fiscal( i_docnum = lv_docnum ).
    CHECK lwa_romaneio-id_interface = '48' AND lwa_romaneio-nro_cg IS NOT INITIAL.

    zcl_carga_saida_insumos=>busca_dados_carga(
     EXPORTING
      i_nr_carga_single        = lwa_romaneio-nro_cg
     IMPORTING
       e_cargas                = DATA(lit_carga)
       e_solicitacoes          = DATA(lit_solicitacoes) ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.
    CHECK sy-subrc EQ 0 AND lit_carga[] IS NOT INITIAL.

    READ TABLE lit_solicitacoes INTO DATA(lwa_sol) WITH KEY vbeln  = lwa_romaneio-vbeln nr_rot = lwa_romaneio-nr_rot.
    CHECK sy-subrc EQ 0 AND lit_solicitacoes[] IS NOT INITIAL.

    "====================================================================
    " Chave NFE
    "====================================================================
    SELECT SINGLE *
      FROM j_1bnfe_active  INTO @ls_act
      WHERE docnum = @lv_docnum.

    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING ls_act TO lv_key.
    lv_chave = lv_key.

    me->at_chave_nfe = lv_chave.

    "Outros Dados
    SELECT SINGLE *
      FROM j_1bnfdoc  INTO @DATA(lwa_doc)
     WHERE docnum = @lv_docnum.

    CHECK sy-subrc EQ 0.

    lva_lifnr = lwa_doc-branch.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lva_lifnr
      IMPORTING
        output = lva_lifnr.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(lwa_lfa1_branch)
      WHERE lifnr EQ @lva_lifnr.

    CHECK sy-subrc EQ 0.


    DO 5 TIMES.


      TRY.
          CLEAR: t_pdf_files[].

          CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
            EXPORTING
              i_docnum = lv_docnum
              i_tipo   = 'XML'
            IMPORTING
              out      = lv_out.

          IF lv_out IS INITIAL.
            CONTINUE.
          ENDIF.

          DATA(lv_xml_base64) = zcl_string=>xstring_to_base64( lv_out ).

          CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
            EXPORTING
              i_docnum = lv_docnum
              i_tipo   = 'PDF'
            IMPORTING
              out      = lv_out_pdf.

          IF lv_out_pdf IS INITIAL.
            CONTINUE.
          ENDIF.

          APPEND INITIAL LINE TO t_pdf_files ASSIGNING FIELD-SYMBOL(<fs_pdf_file>).
          <fs_pdf_file>-data = lv_out_pdf.

          DATA(lit_anexos_carga) = zcl_gos_service_utils=>get_anexos_objeto( EXPORTING
                                                                               i_class_name  =  'ZSDT0112'
                                                                               i_obj_key     =  CONV #( lwa_romaneio-nro_cg ) ).

          LOOP AT lit_anexos_carga ASSIGNING FIELD-SYMBOL(<fs_anexo_carga>).
            TRANSLATE <fs_anexo_carga>-docuclass TO UPPER CASE.
            CHECK <fs_anexo_carga>-docuclass EQ 'PDF'.

            CHECK NOT ( <fs_anexo_carga>-descript CS 'Autorização Embarque' ) AND <fs_anexo_carga>-descript IS NOT INITIAL.

            DATA(lva_xstring_anexo) = zcl_gos_service_utils=>get_xstring_anexo( EXPORTING i_document_id = CONV #( <fs_anexo_carga>-loio_id ) ).

            APPEND INITIAL LINE TO t_pdf_files ASSIGNING <fs_pdf_file>.
            <fs_pdf_file>-data = lva_xstring_anexo.
          ENDLOOP.

          "Agrupa PDF documentos
          TRY.
              IF lines( t_pdf_files ) > 1.
                lv_out_pdf = zcl_faturamento=>zif_faturamento~get_instance(
                                    )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files ).
              ENDIF.
            CATCH zcx_faturamento.
            CATCH zcx_error.
          ENDTRY.

          DATA(lv_pdf_base64) = zcl_string=>xstring_to_base64( lv_out_pdf ).

          IF lv_pdf_base64 IS INITIAL.
            CONTINUE.
          ENDIF.

          EXIT.

        CATCH zcx_doc_eletronico.
          CLEAR: t_pdf_files[].
      ENDTRY.
    ENDDO.

    at_dados-cnpj_depositante = lwa_lfa1_branch-stcd1.
    APPEND INITIAL LINE TO at_dados-lista_notas_fiscais ASSIGNING FIELD-SYMBOL(<fs_nota_fiscal>).

    <fs_nota_fiscal>-numero_pedido_embarque = lwa_carga-nro_pedido_luft.

    IF lwa_sol-seq_carregamento_luft IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_sol-seq_carregamento_luft
        IMPORTING
          output = <fs_nota_fiscal>-sequencia_carregamento.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_sol-seq_entrega
        IMPORTING
          output = <fs_nota_fiscal>-sequencia_carregamento.
    ENDIF.

    CONDENSE: <fs_nota_fiscal>-numero_pedido_embarque,
              <fs_nota_fiscal>-sequencia_carregamento NO-GAPS.

    <fs_nota_fiscal>-numero_nf              = lwa_doc-nfenum.
    <fs_nota_fiscal>-serie_nf               = lwa_doc-series.
    <fs_nota_fiscal>-chave_acesso           = lv_chave.
    <fs_nota_fiscal>-xml_conteudo           = lv_xml_base64.
    <fs_nota_fiscal>-pdf_conteudo           = lv_pdf_base64.


  ENDMETHOD.


  METHOD zif_integracao_outbound~execute_request.

    DATA: lv_docnum TYPE j_1bdocnum.

    r_if_integracao_outbound = me.

    lv_docnum = i_info_request.

    IF zcl_util_sd=>ck_integration_luft_active( i_direcao = 'S' ) EQ abap_false.
      RETURN.
    ENDIF.

    "====================================================================
    " Verificar tipo sementes
    "====================================================================
    SELECT SINGLE matkl, reftyp
     FROM j_1bnflin
     INTO ( @DATA(lv_matkl), @DATA(lv_reftyp) )
     WHERE docnum = @lv_docnum.

    CHECK sy-subrc EQ 0 AND lv_matkl IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0200 INTO @DATA(lwa_zmmt0200)
     WHERE matkl EQ @lv_matkl.

    CHECK sy-subrc EQ 0 AND lwa_zmmt0200-spart EQ '04'. "Sementes

    "====================================================================
    " Ponto de Coleta
    "====================================================================
    CASE lv_reftyp.

      WHEN 'BI'.

        SELECT SINGLE parid
            FROM j_1bnfnad
            INTO @DATA(lv_parid)
            WHERE docnum = @lv_docnum
              AND parvw = 'PC'.

        CHECK sy-subrc EQ 0.

      WHEN 'MD'.

        SELECT SINGLE parid
            FROM j_1bnfnad
            INTO @lv_parid
            WHERE docnum = @lv_docnum
              AND parvw = 'PC'.

        CHECK sy-subrc EQ 0.

      WHEN OTHERS.
        RETURN.

    ENDCASE.

    SELECT SINGLE *
      FROM zsdt0419 INTO @DATA(lwa_ponto_coleta)
      WHERE lifnr = @lv_parid.

    CHECK sy-subrc EQ 0 AND lv_parid IS NOT INITIAL.


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

    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data        = me->at_dados
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      RECEIVING
        r_json      = e_data.


  ENDMETHOD.


  METHOD zif_integracao_outbound~get_id_referencia.

    r_if_integracao_outbound = me.

    e_referencia-id_referencia =  ME->AT_CHAVE_NFE.
    e_referencia-tp_referencia = 'CHAVE_NFE_SAI_LUFT'.

  ENDMETHOD.


  METHOD zif_integracao_outbound~get_instance.

    IF zif_integracao_outbound~at_if_integracao_outbound IS NOT BOUND.
      CREATE OBJECT zif_integracao_outbound~at_if_integracao_outbound TYPE zcl_int_ob_send_nfe_sai_luft.
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
     WHERE service = 'LUFT_INTEGRACAO_NFE_SAIDA'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_erro_geral-msgid
                            msgno = zcx_integracao=>zcx_erro_geral-msgno
                            attr1 = 'Serviço não configurado: '
                            attr2 = 'LUFT_INTEGRACAO_NFE_SAIDA' )
          msgid  = zcx_integracao=>zcx_erro_geral-msgid
          msgno  = zcx_integracao=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = 'Serviço não configurado: '
          msgv2  = 'LUFT_INTEGRACAO_NFE_SAIDA'.
    ENDIF.

    me->zif_integracao_outbound~at_auth_webservice = lwa_webservice.

    CLEAR: me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            = 'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = lwa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_server_protocolo   = abap_off.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             = lwa_webservice-method.
    me->zif_integracao_inject~at_info_request_http-ds_url                = lwa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.
ENDCLASS.
