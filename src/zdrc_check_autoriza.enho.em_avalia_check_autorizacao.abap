METHOD avalia_check_autorizacao .

  DATA: ls_status_request TYPE edoc_br_check_status_req,
        lv_action         TYPE nfe_service_action,
        lv_accesskey      TYPE edoc_accesskey,
        ls_nfeproc        TYPE edoc_br_nfe400,
        ls_cteproc        TYPE edoc_br_cte_if_root,
        lo_parser         TYPE REF TO if_edoc_source_parser,
        lo_params         TYPE REF TO cl_edoc_br_create_entity_param,
        md_xml            TYPE REF TO data,
        ls_digval         TYPE string,
        ls_uf_emit        TYPE zdrct0003-uf_emit,
        ls_modelo         TYPE zdrct0003-modelo.

  FIELD-SYMBOLS: <ls_nfe> TYPE any,
                 <ls_cte> TYPE any.

  ls_status_request = create_request( iv_is_nfe = iv_is_nfe
                                      iv_xml    = iv_xml ).

  IF     ls_status_request-nfe-chnfe IS NOT INITIAL.
    lv_accesskey = ls_status_request-nfe-chnfe.
  ELSEIF ls_status_request-cte-chcte IS NOT INITIAL.
    lv_accesskey = ls_status_request-cte-chcte.
  ENDIF.

  lv_action = retrieve_action_by_key( lv_accesskey ).

*-------------------------------------
* parser xml
*-------------------------------------
  CASE lv_action.
    WHEN cl_nfe_cloud_service_comm=>sc_action-protocol.  "NF-e

      TRY.
          CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
          CREATE OBJECT lo_params
            EXPORTING
              iv_xml = iv_xml.

          lo_parser->load_source( lo_params->get_xml( ) ).
          md_xml   = lo_parser->parse_to_ddic( ls_nfeproc ).

          ASSIGN md_xml->* TO <ls_nfe>.
          ls_nfeproc        = <ls_nfe>.
          ls_modelo         = ls_nfeproc-nfe-infnfe-ide-mod.
          ls_uf_emit        = ls_nfeproc-nfe-infnfe-emit-enderemit-uf.
          ls_digval         = ls_nfeproc-protnfe-infprot-digval.
        CATCH cx_edocument.
          EXIT.
      ENDTRY.

    WHEN 'CHKSTATCTE'.  "CT-e
      TRY.
          CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
          CREATE OBJECT lo_params
            EXPORTING
              iv_xml = iv_xml.

          lo_parser->load_source( lo_params->get_xml( ) ).
          md_xml   = lo_parser->parse_to_ddic( ls_cteproc ).

          ASSIGN md_xml->* TO <ls_cte>.
          ls_cteproc        = <ls_cte>.
          ls_modelo         = ls_cteproc-cte-infcte-ide-mod.
          ls_uf_emit        = ls_cteproc-cte-infcte-emit-enderemit-uf.
          ls_digval         = ls_cteproc-protcte-infprot-digval.

        CATCH cx_edocument.
          EXIT.
      ENDTRY.
  ENDCASE.

*-----------------------------
* validar UF's Desabilitadas p/ Check Autorização SEFAZ
*-----------------------------
  SELECT SINGLE *
    FROM zdrct0003
    INTO @DATA(w_zdrt0003)
   WHERE uf_emit = @ls_uf_emit
     AND modelo  = @ls_modelo.

  IF sy-subrc = 0.
    rs_result-cstat  = '100'.
    rs_result-digval = ls_digval.
  ENDIF.

ENDMETHOD.
