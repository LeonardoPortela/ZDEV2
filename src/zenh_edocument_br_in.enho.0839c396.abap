"Name: \TY:CL_EDOCUMENT_BR_IN\ME:UPDATE_WITH_SEFAZ_RESPONSE\SE:END\EI
ENHANCEMENT 0 ZENH_EDOCUMENT_BR_IN.

DATA:
  rs_edocumentfile            TYPE edocumentfile,
  lv_prefix                   TYPE string,
  lo_create_param             TYPE REF TO cl_edoc_br_create_entity_param,
  lo_nfe_entity               TYPE REF TO cl_edoc_br_nfe_entity,
  lo_nfeinbr_creator          TYPE REF TO if_edoc_br_create_entity,
  lv_dest_cnpj                TYPE edoc_supplier,
  ls_bapiret2                 TYPE bapiret2,
  lt_bapiret2                 TYPE bapirettab,
  lx_transformation           TYPE REF TO cx_nfe_transformation,
  lx_nfe_cloud_response_error TYPE REF TO cx_edoc_br_cloud_response_err
  .
TRY.

    IF ms_edobrincoming IS NOT INITIAL AND ms_edobrincoming-company_code IS INITIAL.
      " recupear o arquivo XML
      SELECT * FROM edocumentfile
        INTO rs_edocumentfile
        UP TO 1 ROWS
        WHERE edoc_guid = ms_edobrincoming-edoc_guid
          AND  file_type = sc_nfe_xml
        ORDER BY seq_no DESCENDING.
      ENDSELECT.

      CREATE OBJECT lo_nfeinbr_creator TYPE cl_edoc_br_nfe_entity_creator.

      CREATE OBJECT lo_create_param
        EXPORTING
          iv_xml = rs_edocumentfile-file_raw.

      lo_nfe_entity ?= lo_nfeinbr_creator->create( lo_create_param ).

      IF lo_nfe_entity IS BOUND.
        lv_dest_cnpj = lo_nfe_entity->retrieve_dest_cnpj( ).
      ENDIF.

      DATA(lv_cgc_company) = lv_dest_cnpj(8).

      SELECT bukrs FROM t001z
        INTO  TABLE @DATA(lt_bukrs)
        WHERE party = 'J_1BCG'
        AND   paval = @lv_cgc_company.

      IF sy-subrc NE 0.

        IF ls_edocumentfile-file_type = 'RECEVT_XML'.

          REPLACE 'recepcaoEvento'  IN ls_edocumentfile-file_name WITH 'nfeTercSemEvento'.

          attach_file(
            is_edocumentfile  = ls_edocumentfile
            iv_update_history = iv_update_history
          ).
        ENDIF. " tipo de arquivo => apenas recevt..

      ENDIF. " verificação na tabela de empresas -> CNPJ de terceiro

    ENDIF. " Edocument de terceiro ( company-code deve estar em branco)

  CATCH cx_nfe_transformation INTO lx_transformation.
    ls_bapiret2 = cl_edoc_br_bapiret_utils=>build_bapiret_from_t100( lx_transformation ).
    APPEND ls_bapiret2 TO lt_bapiret2.
  CATCH cx_edoc_br_cloud_response_err INTO lx_nfe_cloud_response_error.
    lt_bapiret2 = lx_nfe_cloud_response_error->build_bapiret_display( ).
ENDTRY.

ENDENHANCEMENT.
