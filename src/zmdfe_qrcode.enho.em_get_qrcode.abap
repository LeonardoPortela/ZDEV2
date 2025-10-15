METHOD get_qrcode .

  DATA: lx_nfe              TYPE REF TO cx_nfe,
        ls_req_qr_code      TYPE nfe_cloud_cte_req_qr_code,
        ls_response         TYPE nfe_cloud_com_response,
        ls_resp_qr_code     TYPE nfe_cloud_cte_resp_qr_code,
        ls_company_code_def TYPE j_1bnfe_s_company_code_def.

  TRY .

      ls_req_qr_code-access_key        = get_access_key( is_electronic_document ).
      ls_req_qr_code-issuer_identifier = is_electronic_document-stcd1.
      ls_req_qr_code-issuing_state     = mo_region_map->ufcode_to_region( is_electronic_document-regio ).
      ls_req_qr_code-environment_type  = cl_nfe_cloud_cte_enum_envrmt=>get_environment_type_descr(
                                           cl_nfe_field_formatter=>convert_any_to_string( iv_environment ) ).
      ls_req_qr_code-document_type     = cl_nfe_cloud_constant_doc_type=>mdfe.
*     ls_req_qr_code-has_signature     = should_sign_qrcode( is_electronic_document-tpemis ).
      ls_req_qr_code-has_signature     = 'true'.

      MOVE-CORRESPONDING is_document_header  TO ls_company_code_def.

      ls_response = send_request_json( iv_service            = cl_nfe_cloud_constant_service=>qr_code
                                       ia_request            = ls_req_qr_code
                                       is_company_definition = ls_company_code_def ).

      IF ls_response-http_status = cl_nfe_constant_http_status=>ok.
        mo_json_transformer->deserialize( EXPORTING
                                            json        = ls_response-content
                                            pretty_name = cl_nfe_json_transformation=>pretty_mode-camel_case
                                          CHANGING
                                            data        = ls_resp_qr_code ).
        rv_qr_code_url = ls_resp_qr_code-qr_code_url.
      ENDIF.

    CATCH cx_nfe_cloud_com INTO lx_nfe.
      mo_error_handler->handle_error_with_exception( iv_document_number = is_electronic_document-docnum
                                                     ix_nfe             = lx_nfe ).

  ENDTRY.

ENDMETHOD.
