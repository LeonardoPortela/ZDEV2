*&---------------------------------------------------------------------*
*& Report  Z_ENVIA_XML_LEGADO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_envia_xml_legado.

TABLES: zib_nfe_dist_ter.

SELECTION-SCREEN BEGIN OF BLOCK nfedata WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: pdocnum FOR zib_nfe_dist_ter-docnum_nfe,
                  pchavec FOR zib_nfe_dist_ter-chave_nfe.

SELECTION-SCREEN END OF BLOCK nfedata.

SELECTION-SCREEN BEGIN OF BLOCK disp WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_sisamg TYPE c AS CHECKBOX,
              p_luft   TYPE c AS CHECKBOX,
              p_ecomme TYPE c AS CHECKBOX,
              p_todos  TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK disp.

PARAMETERS: pdocsend TYPE zib_nfe_dist_ter-docnum_nfe NO-DISPLAY.

INITIALIZATION.

  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.

START-OF-SELECTION.

  DATA: lv_rfcdest    TYPE rfcdest.
  DATA: lv_rfcdest_aux TYPE rfcdest.
  DATA: lv_xnfeactive TYPE j_1bxnfeactive.
  DATA: lo_download   TYPE REF TO zcl_j_1bnfe_xml_download. "*-S4H-US 122597-07.09.2023-JT
  DATA: lo_dom        TYPE REF TO if_ixml_document.
  DATA: v_dt_limite   TYPE sy-datum.

  DATA: wa_j_1bnfdoc TYPE j_1bnfdoc,
        wa_j_1bnflin TYPE j_1bnflin,
        lc_romaneio  TYPE zsdt0001,
        lc_remessa   TYPE xblnr,
        lc_direcao   TYPE char3.

  CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
    EXPORTING
      i_bukrs      = '0001'
      i_branch     = '0155'
      i_model      = '55'
    IMPORTING
      e_rfcdest    = lv_rfcdest
      e_xnfeactive = lv_xnfeactive.

  DATA: gv_rfcdest     TYPE rfcdest.
  DATA: gv_rfcdest_aux TYPE rfcdest.
  gv_rfcdest = lv_rfcdest.

  DATA: ob_web_service TYPE REF TO zcl_webservice.
  DATA: ob_web_cancel  TYPE REF TO zcl_webservice.

  IF pdocsend IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_zib_autorit_grc)
      FROM zib_autorit_grc
     WHERE docnum EQ @pdocsend.

  ELSEIF pdocnum[] IS INITIAL AND pchavec[] IS INITIAL.

    DATA: e_data_limite TYPE datum.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = 2
        months    = 0
        years     = 0
        signum    = '-'
      IMPORTING
        calc_date = e_data_limite.

    "Verificar Documentos Cancelados e Não estão na Tabela
    SELECT * INTO TABLE @DATA(it_j_1bnfe_active)
      FROM j_1bnfe_active AS j
     WHERE j~authdate GE @e_data_limite
       AND j~cancel   EQ 'X'
       AND j~code     EQ '101'
       AND j~form     NE @space
       AND NOT EXISTS ( SELECT * FROM zib_autorit_grc AS r WHERE r~docnum EQ j~docnum AND r~code EQ '101' ).

    DATA: lc_autorizacao TYPE zib_autorit_grc.

    LOOP AT it_j_1bnfe_active INTO DATA(wa_j_1bnfe_active).
      CLEAR: lc_autorizacao.
      lc_autorizacao-chnfe = wa_j_1bnfe_active-regio  && wa_j_1bnfe_active-nfyear  && wa_j_1bnfe_active-nfmonth &&
                             wa_j_1bnfe_active-stcd1  && wa_j_1bnfe_active-model   && wa_j_1bnfe_active-serie &&
                             wa_j_1bnfe_active-nfnum9 && wa_j_1bnfe_active-docnum9 && wa_j_1bnfe_active-cdv.

      lc_autorizacao-dt_registro   = sy-datum.
      lc_autorizacao-hr_registro   = sy-uzeit.
      lc_autorizacao-us_registro   = sy-uname.
      lc_autorizacao-rg_atualizado = '0'.
      lc_autorizacao-direction     = 'OUTB'.
      lc_autorizacao-docnum        = wa_j_1bnfe_active-docnum.
      lc_autorizacao-code          = wa_j_1bnfe_active-code.
      lc_autorizacao-cancel        = abap_true.
      MODIFY zib_autorit_grc FROM lc_autorizacao.
    ENDLOOP.

    IF it_j_1bnfe_active[] IS NOT INITIAL.
      COMMIT WORK AND WAIT.
    ENDIF.

    SELECT * INTO TABLE @it_zib_autorit_grc
      FROM zib_autorit_grc
     WHERE rg_atualizado EQ '0'.

  ELSE.

    SELECT * INTO TABLE @it_zib_autorit_grc
      FROM zib_autorit_grc
     WHERE docnum IN @pdocnum
       AND chnfe  IN @pchavec.

  ENDIF.

  LOOP AT it_zib_autorit_grc INTO DATA(wl_xml_valid).

    DATA(tabix) = sy-tabix.

    DATA(_deletar) = abap_false.

    IF strlen( wl_xml_valid-chnfe ) NE 44.
      _deletar = abap_true.
    ELSEIF wl_xml_valid-chnfe+20(2) NE '55'.
      _deletar = abap_true.
    ENDIF.

    IF _deletar EQ abap_false AND wl_xml_valid-cancel = abap_false.
      SELECT SINGLE * INTO @DATA(wa_zib_autorit_canc)
       FROM zib_autorit_grc
      WHERE chnfe   EQ @wl_xml_valid-chnfe
        AND cancel  EQ @abap_true.

      IF sy-subrc IS INITIAL.
        _deletar = abap_true.
      ENDIF.
    ENDIF.

    IF _deletar EQ abap_true.
      wl_xml_valid-rg_atualizado = '1'.
      MODIFY zib_autorit_grc FROM wl_xml_valid.
      COMMIT WORK.
      DELETE it_zib_autorit_grc INDEX tabix.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  CHECK it_zib_autorit_grc[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_j_1bnflin)
    FROM j_1bnflin
     FOR ALL ENTRIES IN @it_zib_autorit_grc
   WHERE docnum EQ @it_zib_autorit_grc-docnum.

  SORT it_j_1bnflin BY docnum.
  DELETE ADJACENT DUPLICATES FROM it_j_1bnflin COMPARING docnum.

  DATA: out_binxml TYPE xstring.
  DATA: w_size     TYPE i.
  DATA: t_xml      TYPE dcxmllines.

  SELECT SINGLE *
    FROM tvarvc INTO @DATA(sinc_xml_legado)
   WHERE name = 'SINC_XML_NFE_LEGADO_STOP'.

  IF sy-subrc EQ 0.
    v_dt_limite = sy-datum - 2.

    LOOP AT it_zib_autorit_grc INTO DATA(wa_xml_upd) WHERE dt_registro < v_dt_limite.
      wa_xml_upd-rg_atualizado = '3'.
      MODIFY zib_autorit_grc FROM wa_xml_upd.
      COMMIT WORK AND WAIT.
      CONTINUE.
    ENDLOOP.
  ENDIF.

  LOOP AT it_zib_autorit_grc INTO DATA(wa_xml).

    CLEAR: out_binxml, w_size, t_xml.

    " 24.04.2023 - 102324 - RBL -->
    IF p_todos EQ abap_true OR p_ecomme EQ abap_true. "WPP #172204
      CALL FUNCTION 'ZSDMF_ENVIAR_XML_ECOMMERCE'
        EXPORTING
          i_docnum = wa_xml-docnum.
    ENDIF.
    " 24.04.2023 - 102324 - RBL --<

    IF p_todos EQ abap_true OR p_luft EQ abap_true. "WPP #172204
      "Panf #Projeto insumos 2025 - inicio
      PERFORM f_enviar_nfe_sai_luft USING wa_xml.
      "Panf #Projeto insumos 2025 - fim

      "FF #172204 - inicio
      PERFORM f_enviar_nfe_ent_luft USING wa_xml.
      "FF #172204 - fim
    ENDIF.

    IF p_todos EQ abap_true OR p_sisamg EQ abap_true. "WPP #172204
      PERFORM f_enviar_sistemas_amaggi USING wa_xml. "WPP #172204
    ENDIF.

  ENDLOOP.

FORM f_enviar_nfe_ent_luft USING p_wa_xml STRUCTURE zib_autorit_grc.

  CHECK p_wa_xml-direction = 'INBD'.

  TRY.
      zcl_int_ob_send_nfe_ent_luft=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request =  p_wa_xml-chnfe ).
    CATCH zcx_integracao.
    CATCH zcx_error.
  ENDTRY.

ENDFORM.


FORM f_enviar_nfe_sai_luft USING p_wa_xml STRUCTURE zib_autorit_grc.

  CHECK p_wa_xml-direction = 'OUTB'.

  TRY.
      zcl_int_ob_send_nfe_sai_luft=>zif_integracao_outbound~get_instance(
      )->execute_request(
        EXPORTING
          i_info_request           =  p_wa_xml-docnum

      ).
    CATCH zcx_integracao.
    CATCH zcx_error.
  ENDTRY.


ENDFORM.

FORM f_enviar_sistemas_amaggi USING wa_xml STRUCTURE zib_autorit_grc.

  IF wa_xml-xml_com_erro EQ abap_true. "XML Recebido com Erro

    SELECT SINGLE *
      FROM setleaf INTO @DATA(lwa_int_dfe_legado)
     WHERE setname EQ 'INTEGRACAO_DFE_LEGADO'
       AND valfrom EQ 'XML_NFE_ERRO'.

    IF sy-subrc NE 0.
      wa_xml-rg_atualizado = '1'.
      MODIFY zib_autorit_grc FROM wa_xml.
      COMMIT WORK AND WAIT.
      RETURN.
    ENDIF.

    "Se possuir integração com sucesso, não integrar XML com erro
    SELECT SINGLE * INTO @DATA(lwa_zib_autorit_grc_0100)
      FROM zib_autorit_grc
     WHERE chnfe EQ @wa_xml-chnfe
       AND code  EQ '100'.

    IF sy-subrc IS INITIAL.
      wa_xml-rg_atualizado = '1'.
      MODIFY zib_autorit_grc FROM wa_xml.
      COMMIT WORK AND WAIT.
      RETURN.
    ENDIF.

    CHECK wa_xml-ds_erro IS NOT INITIAL.

    wa_xml-ds_erro = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( wa_xml-ds_erro ) ) ).

*-S4H-US 122597-07.09.2023-JT-inicio
    " Instantiate download object
*      CREATE OBJECT lo_download
*        EXPORTING
*          iv_xml_key = wa_xml-chnfe
*          iv_rfc     = gv_rfcdest.
*-S4H-US 122597-07.09.2023-JT-fim

    CASE wa_xml-chnfe+20(2).
      WHEN '55'.
        DATA(c_doctype_erro) = 'NFE'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

*-S4H-US 122597-07.09.2023-JT-inicio
*      TRY.
*          lo_download->load_xml_content( EXPORTING iv_direction = wa_xml-direction iv_doctype = c_doctype_erro ).
*        CATCH zcx_error INTO DATA(ex_erro_xml).
*          ex_erro_xml->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
*      ENDTRY.
*
    "DATA(o_xml_content_erro) = lo_download->get_xml_content( ).

    lc_direcao = COND #( WHEN wa_xml-direction = 'INBD' THEN 'IN'
                                                        ELSE 'OUT' ).

    zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = CONV #( wa_xml-chnfe )
                                                           i_direcao = CONV #( lc_direcao )
                                                 IMPORTING e_xml_raw = DATA(o_xml_content_erro) ).

*-S4H-US 122597-07.09.2023-JT-fim

    CHECK o_xml_content_erro IS NOT INITIAL.

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml           = o_xml_content_erro
      IMPORTING
        document      = lo_dom
      EXCEPTIONS
        invalid_input = 1
        OTHERS        = 2.

    CHECK sy-subrc IS INITIAL.

    "Convert DOM to XML doc (table)
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

    CHECK sy-subrc IS INITIAL.

    DATA(xml_string_base64_erro) = zcl_string=>xstring_to_base64( out_binxml ).

    DATA(json_out_erro) = '{ ' &&
                              '"chave":' && '"' && wa_xml-chnfe && '", ' &&
                              '"xml":' && '"' && xml_string_base64_erro && '", ' &&
                              '"tipoStatus":' && '"' && 999 && '", ' &&
                              '"descricaoErroRecebimento": ' && '"' && wa_xml-ds_erro && '" ' &&
                           '}'.

    CREATE OBJECT ob_web_service.
    ob_web_service->zif_webservice~autentica_opus = abap_true.
    ob_web_service->set_tipo( i_tipo = 'O' ).
    ob_web_service->set_servico( i_servico = 'XM' ).
    DATA(var_url_xml)  = ob_web_service->get_uri( ) && '/incluir'.
    DATA(var_http_xml) = ob_web_service->url( i_url = CONV #( var_url_xml ) ).

    var_http_xml->request->set_header_field(
      EXPORTING
        name  = 'Content-Type'
        value = 'application/json; charset=UTF-8'
    ).

    ob_web_service->zif_webservice~consultar(
      EXPORTING
        i_http                     = var_http_xml
        i_xml                      = json_out_erro
      IMPORTING
        e_code                     = DATA(e_code_ret)
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc IS NOT INITIAL.
      CLEAR: ob_web_service.
      RETURN.
    ENDIF.

    IF e_code_ret NE '200'.
      CLEAR: ob_web_service.
      RETURN.
    ENDIF.

    CLEAR: ob_web_service.

    wa_xml-rg_atualizado = '1'.
    MODIFY zib_autorit_grc FROM wa_xml.
    COMMIT WORK AND WAIT.

    RETURN.

  ENDIF. "Fim XML Recebido com Erro

  IF wa_xml-direction EQ 'INBD'.
    SELECT SINGLE * INTO @DATA(wa_zib_autorit_grc)
      FROM zib_autorit_grc
     WHERE chnfe EQ @wa_xml-chnfe
       AND direction EQ 'OUTB'.

    IF sy-subrc IS INITIAL.
      wa_xml-rg_atualizado = '1'.
      MODIFY zib_autorit_grc FROM wa_xml.
      COMMIT WORK AND WAIT.
      RETURN.
    ENDIF.
  ENDIF.

  CASE wa_xml-cancel.
    WHEN abap_true.

*-S4H-US 122597-07.09.2023-JT-inicio
      " Instantiate download object
*        CREATE OBJECT lo_download
*          EXPORTING
*            iv_xml_key = wa_xml-chnfe
*            iv_rfc     = gv_rfcdest.
*-S4H-US 122597-07.09.2023-JT-fim

      CASE wa_xml-chnfe+20(2).
        WHEN '55'.
          DATA(c_doctype) = 'NFE'.
        WHEN '57'.
          c_doctype = 'CTE'.
        WHEN '58'.
          c_doctype = 'MDF'.
      ENDCASE.

*-S4H-US 122597-07.09.2023-JT-inicio
*        TRY .
*            lo_download->load_xml_content(
*                EXPORTING
*                  iv_direction = wa_xml-direction
*                  iv_doctype = c_doctype
*                  iv_event_type = '110111'
*                  iv_event_seqnum = '01'
*                  iv_mass_dl_type = 'E' ).
*          CATCH zcx_error INTO DATA(ex_erro).
*            ex_erro->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
*        ENDTRY.
*
*        DATA(o_xml_content) = lo_download->get_xml_content( ).

      lc_direcao = COND #( WHEN wa_xml-direction = 'INBD' THEN 'IN'
                                                          ELSE 'OUT' ).
      zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = CONV #( wa_xml-chnfe )
                                                             i_direcao = CONV #( lc_direcao )
                                                             i_evento  = '110111'
                                                   IMPORTING e_xml_raw = DATA(o_xml_content) ).
*-S4H-US 122597-07.09.2023-JT-fim

      IF o_xml_content IS INITIAL.
        RETURN.
      ENDIF.

      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          xml           = o_xml_content
        IMPORTING
          document      = lo_dom
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.

      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.

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

      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.

      DATA(xml_string_base64) = zcl_string=>xstring_to_base64( out_binxml ).

      DATA(json_cancel) = '{ ' &&
                          '"chave":' && '"' && wa_xml-chnfe && '", ' &&
                          '"tipoStatus":' && '"' && wa_xml-code && '", ' &&
                          '"xml":' && '"' && xml_string_base64 && '", ' &&
                          '"docnum": ' && '"' && wa_xml-docnum && '" ' &&
                          '}'.

      CREATE OBJECT ob_web_cancel.
      ob_web_cancel->zif_webservice~autentica_opus = abap_true.
      ob_web_cancel->set_tipo( i_tipo = 'O' ).
      ob_web_cancel->set_servico( i_servico = 'XM' ).
      DATA(var_url_cancel)  = ob_web_cancel->get_uri( ) && '/cancelar'.
      DATA(var_http_cancel) = ob_web_cancel->url( i_url = CONV #( var_url_cancel ) ).

      var_http_cancel->request->set_header_field(
        EXPORTING
          name  = 'Content-Type'
          value = 'application/json; charset=UTF-8'
      ).

      ob_web_cancel->zif_webservice~consultar(
        EXPORTING
          i_http                     = var_http_cancel
          i_xml                      = json_cancel
        IMPORTING
          e_code                     = DATA(e_code)
          e_reason                   = DATA(e_reason)
        RECEIVING
          e_resultado                = DATA(json_retorno)
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).

      IF sy-subrc IS NOT INITIAL.
        CLEAR: ob_web_cancel.
        RETURN.
      ENDIF.

      IF e_code NE '200'.
        CLEAR: ob_web_cancel.
        RETURN.
      ENDIF.

      CLEAR: ob_web_cancel.
      wa_xml-rg_atualizado = '1'.
      MODIFY zib_autorit_grc FROM wa_xml.
      COMMIT WORK AND WAIT.

    WHEN abap_false.

*-S4H-US 122597-07.09.2023-JT-inicio
      " Instantiate download object
*        CREATE OBJECT lo_download
*          EXPORTING
*            iv_xml_key = wa_xml-chnfe
*            iv_rfc     = gv_rfcdest.
*-S4H-US 122597-07.09.2023-JT-fim

      CASE wa_xml-chnfe+20(2).
        WHEN '55'.
          c_doctype = 'NFE'.
        WHEN '57'.
          c_doctype = 'CTE'.
        WHEN '58'.
          c_doctype = 'MDF'.
      ENDCASE.

*-S4H-US 122597-07.09.2023-JT-inicio
*        TRY .
*            lo_download->load_xml_content( EXPORTING iv_direction = wa_xml-direction iv_doctype = c_doctype ).
*          CATCH zcx_error INTO data(ex_erro).
*            ex_erro->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
*        ENDTRY.
*
*        o_xml_content = lo_download->get_xml_content( ).

      CLEAR : o_xml_content.
      lc_direcao = COND #( WHEN wa_xml-direction = 'INBD' THEN 'IN'
                                                          ELSE 'OUT' ).
      zcl_drc_utils=>get_xml_documento_eletronico( EXPORTING i_chave   = CONV #( wa_xml-chnfe )
                                                             i_direcao = CONV #( lc_direcao )
                                                   IMPORTING e_xml_raw = o_xml_content  ).
*-S4H-US 122597-07.09.2023-JT-fim

      IF o_xml_content IS INITIAL.
        RETURN.
      ENDIF.

      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          xml           = o_xml_content
        IMPORTING
          document      = lo_dom
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.

      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.

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

      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.

      CLEAR: wa_j_1bnfdoc, wa_j_1bnflin, lc_romaneio, lc_remessa.

      IF wa_xml-direction NE 'INBD'.

        SELECT SINGLE * INTO wa_j_1bnfdoc
          FROM j_1bnfdoc
         WHERE docnum EQ wa_xml-docnum.

        IF sy-subrc IS NOT INITIAL.
          wa_xml-rg_atualizado = '1'.
          MODIFY zib_autorit_grc FROM wa_xml.
          COMMIT WORK AND WAIT.
          RETURN.
        ENDIF.

        IF wa_j_1bnfdoc-cancel EQ 'X'.
          wa_xml-rg_atualizado = '1'.
          MODIFY zib_autorit_grc FROM wa_xml.
          COMMIT WORK AND WAIT.
          RETURN.
        ENDIF.

        READ TABLE it_j_1bnflin WITH KEY docnum = wa_xml-docnum INTO wa_j_1bnflin BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR: wa_j_1bnflin.
        ENDIF.

        SELECT SINGLE * INTO lc_romaneio
          FROM zsdt0001
         WHERE nro_nf_prod EQ wa_xml-docnum
           AND nro_nf_prod NE space.

        IF sy-subrc IS NOT INITIAL AND wa_j_1bnflin IS NOT INITIAL.
          SELECT SINGLE * INTO lc_romaneio
            FROM zsdt0001
           WHERE nro_nf_rem EQ wa_xml-docnum
             AND nro_nf_rem NE space.
        ENDIF.

        IF sy-subrc IS NOT INITIAL AND wa_j_1bnflin IS NOT INITIAL.

          SELECT SINGLE * INTO lc_romaneio
            FROM zsdt0001
           WHERE fatura_prod EQ wa_j_1bnflin-refkey(10)
             AND fatura_prod NE space.

          IF sy-subrc IS NOT INITIAL.
            CASE wa_j_1bnflin-reftyp.
              WHEN 'BI'.

                SELECT SINGLE vbelv INTO lc_remessa
                  FROM vbfa
                 WHERE vbeln   EQ wa_j_1bnflin-refkey(10)
                   AND vbtyp_n EQ 'M'
                   AND vbtyp_v EQ 'J'.

                IF sy-subrc IS INITIAL AND lc_remessa IS NOT INITIAL.
                  SELECT SINGLE * INTO @lc_romaneio
                    FROM zsdt0001
                   WHERE doc_rem EQ @lc_remessa
                     AND doc_rem NE @space.

                  IF sy-subrc IS NOT INITIAL.

                    SELECT COUNT(*)
                      FROM likp
                      WHERE vbeln EQ lc_remessa
                      AND tcode = 'ZLES0136'.

                    IF sy-subrc IS INITIAL.
                      RETURN.
                    ENDIF.

                    CLEAR: lc_romaneio, lc_remessa.

                  ENDIF.
                ENDIF.

              WHEN 'MD'.

                SELECT SINGLE xblnr_mkpf INTO lc_remessa
                  FROM mseg
                 WHERE mblnr EQ wa_j_1bnflin-refkey(10)
                   AND mjahr EQ wa_j_1bnflin-refkey+10(4)
                   AND zeile EQ '0001'.

                IF sy-subrc IS INITIAL AND lc_remessa IS NOT INITIAL.
                  SELECT SINGLE * INTO lc_romaneio
                    FROM zsdt0001
                   WHERE doc_rem EQ lc_remessa
                     AND doc_rem NE space.

                  IF sy-subrc IS NOT INITIAL.

                    SELECT COUNT(*)
                      FROM likp
                      WHERE vbeln EQ lc_remessa
                      AND tcode = 'ZLES0136'.

                    IF sy-subrc IS INITIAL.
                      RETURN.
                    ENDIF.

                    CLEAR: lc_romaneio, lc_remessa.

                  ENDIF.

                ENDIF.

            ENDCASE.
          ENDIF.

        ENDIF.

        CASE wa_j_1bnfdoc-direct.
          WHEN '1'.
            DATA(lc_tiponota) = 'E'.
          WHEN '2'.
            lc_tiponota = 'S'.
          WHEN '3'.
            lc_tiponota = 'E'.
          WHEN '4'.
            lc_tiponota = 'S'.
        ENDCASE.

      ELSE.
        lc_tiponota = 'E'.
      ENDIF.

      xml_string_base64 = zcl_string=>xstring_to_base64( out_binxml ).

      IF zcl_string=>length( text = CONV #( lc_romaneio-ch_referencia ) ) GT 11.
        CLEAR: lc_romaneio-ch_referencia.
      ENDIF.

      DATA(json_out) = '{ ' &&
                          '"chave":' && '"' && wa_xml-chnfe && '", ' &&
                          '"xml":' && '"' && xml_string_base64 && '", ' &&
                          '"seqPlaRomaneio":' && '"' && lc_romaneio-ch_referencia && '", ' &&
                          '"numeroRemessa":' && '"' && lc_romaneio-doc_rem && '", ' &&
                          '"tipoNota":' && '"' && lc_tiponota && '", ' &&
                          '"tipoStatus":' && '"' && 100 && '", ' &&
                          '"docnum": ' && '"' && wa_xml-docnum && '", ' &&
                          '"ncm": ' && '"' && wa_j_1bnflin-nbm && '" ' &&
                       '}'.

      CREATE OBJECT ob_web_service.
      ob_web_service->zif_webservice~autentica_opus = abap_true.
      ob_web_service->set_tipo( i_tipo = 'O' ).
      ob_web_service->set_servico( i_servico = 'XM' ).
      DATA(var_url)  = ob_web_service->get_uri( ) && '/incluir'.
      DATA(var_http) = ob_web_service->url( i_url = CONV #( var_url ) ).

      var_http->request->set_header_field(
        EXPORTING
          name  = 'Content-Type'
          value = 'application/json; charset=UTF-8'
      ).

      ob_web_service->zif_webservice~consultar(
        EXPORTING
          i_http                     = var_http
          i_xml                      = json_out
        IMPORTING
          e_code                     = e_code
          e_reason                   = e_reason
        RECEIVING
          e_resultado                = json_retorno
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).

      IF sy-subrc IS NOT INITIAL.
        CLEAR: ob_web_service.
        RETURN.
      ENDIF.

      IF e_code NE '200'.
        CLEAR: ob_web_service.
        RETURN.
      ENDIF.

      CLEAR: ob_web_service.

      wa_xml-rg_atualizado = '1'.
      MODIFY zib_autorit_grc FROM wa_xml.
      COMMIT WORK AND WAIT.
  ENDCASE.



ENDFORM.
