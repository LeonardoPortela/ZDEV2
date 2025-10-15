"Name: \TY:CL_EDOC_BR_PROC_RECEVENTO\ME:EXECUTE\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_BR_PROC_RECEVENTO.

  BREAK rblima.

  DATA: lv_dest_cnpj                TYPE edoc_supplier,
        ls_bapiret2                 TYPE bapiret2,
        lt_bapiret2                 TYPE bapirettab,
        lx_transformation           TYPE REF TO cx_nfe_transformation,
        lx_nfe_cloud_response_error TYPE REF TO cx_edoc_br_cloud_response_err,
        lv_xml                      TYPE edoc_file,
        ls_sefaz_response           TYPE edoc_br_recepcaoevento_resp.

  TRY.
      IF is_edobrincoming IS NOT INITIAL AND is_edobrincoming-company_code IS INITIAL.

        lv_dest_cnpj = get_dest_cnpj( iv_edoc_guid = is_edobrincoming-edoc_guid ) .

        IF zcl_nfe_aux=>verificar_cnpj_terceiro( CONV #( lv_dest_cnpj ) ) = abap_true.
          " alimentar o RESPONSE
          rs_response-cstat = '136'. " 136 - Evento registrado, mas não vinculado a NF-e
          RETURN.
        ENDIF.

      ENDIF.

    CATCH cx_nfe_transformation INTO lx_transformation.
      ls_bapiret2 = cl_edoc_br_bapiret_utils=>build_bapiret_from_t100( lx_transformation ).
      APPEND ls_bapiret2 TO lt_bapiret2.
    CATCH cx_edoc_br_cloud_response_err INTO lx_nfe_cloud_response_error.
      lt_bapiret2 = lx_nfe_cloud_response_error->build_bapiret_display( ).
  ENDTRY.

  IF lt_bapiret2 IS NOT INITIAL.
    RAISE EXCEPTION TYPE cx_edoc_br_proc
      EXPORTING
        mt_bapiret = lt_bapiret2.
  ENDIF.

ENDENHANCEMENT.
