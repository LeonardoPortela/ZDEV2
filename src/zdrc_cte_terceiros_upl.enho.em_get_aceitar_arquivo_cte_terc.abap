METHOD get_aceitar_arquivo_cte_terc .

  DATA: ls_cteproc        TYPE edoc_br_cte_if_root,
        lo_parser         TYPE REF TO if_edoc_source_parser,
        lo_params         TYPE REF TO cl_edoc_br_create_entity_param,
        md_xml            TYPE REF TO data,
        ls_cnpj_recebedor TYPE j_1bcgc,
        ls_cnpj_expedidor TYPE j_1bcgc,
        ls_raiz_cnpj(8)   TYPE n.

  FIELD-SYMBOLS: <ls_cte> TYPE any.

  CHECK iv_mod = '57'.

*-------------------------------------
* Parse XML
*-------------------------------------
  TRY.
      CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
      CREATE OBJECT lo_params
        EXPORTING
          iv_xml = mo_xml.

      lo_parser->load_source( lo_params->get_xml( ) ).
      md_xml   = lo_parser->parse_to_ddic( ls_cteproc ).

      ASSIGN md_xml->*    TO <ls_cte>.
      ls_cteproc           = <ls_cte>.
      ls_cnpj_recebedor    = ls_cteproc-cte-infcte-receb-cnpj.
      ls_cnpj_expedidor    = ls_cteproc-cte-infcte-exped-cnpj.

    CATCH cx_edocument.
      EXIT.
  ENDTRY.

*-------------------------------------
* Raiz CNPJ
*-------------------------------------
  CASE iv_toma.
    WHEN '0'.
      ls_raiz_cnpj = iv_emit_cnpj(8).
    WHEN '1'.
      ls_raiz_cnpj = ls_cnpj_expedidor(8).
    WHEN '2'.
      ls_raiz_cnpj = ls_cnpj_recebedor(8).
    WHEN '3'.
      ls_raiz_cnpj = iv_dest_cnpj(8).
  ENDCASE.

  CHECK ls_raiz_cnpj IS NOT INITIAL.

  SELECT SINGLE *
    INTO @DATA(_t001z)
    FROM t001z
   WHERE party = 'J_1BCG'
     AND paval = @ls_raiz_cnpj.

  CHECK sy-subrc <> 0.

*-------------------------------------
* retorno metodo
*-------------------------------------
  rs_search_parameters-company_code = zcl_nfe_aux=>get_bukrs_terceiro( ).
  rs_search_parameters-plant        = zcl_nfe_aux=>get_werks_terceiro( ).
  rs_search_parameters-cfop         = iv_cfop.
  rs_search_parameters-toma         = iv_toma.
  rs_search_parameters-mod          = iv_mod.

ENDMETHOD.
