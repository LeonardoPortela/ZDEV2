"Name: \TY:CL_EDOC_BR_CREATE_EDOCUMENT\ME:FILL_SEARCH_PARAMETERS\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_EDOC_BR_CREATE_EDOCUMENT.

  " 13.11.2023 - 127332 - RBL -->

  DATA: lva_dest_cnpj_cpf TYPE j_1bcgc.
  lva_dest_cnpj_cpf = iv_dest_cnpj.
  EXPORT lva_dest_cnpj_cpf FROM lva_dest_cnpj_cpf TO MEMORY ID 'CNPJ_CPF_DEST_TERCEIRO'.

  IF iv_accesskey+20(2) = '55' OR
     iv_accesskey+20(2) = '57' OR
     iv_accesskey+20(2) = '67'.

    IF zcl_nfe_aux=>verificar_cnpj_terceiro( iv_dest_cnpj ) = abap_true.

      rs_search_parameters-company_code = zcl_nfe_aux=>get_bukrs_terceiro( ).
      rs_search_parameters-plant  = zcl_nfe_aux=>get_werks_terceiro( ).
      rs_search_parameters-cfop	= iv_cfop.
      rs_search_parameters-toma	= iv_toma.
      rs_search_parameters-mod 	= iv_mod.

      RETURN.

    ENDIF.
  ENDIF.
  " 13.11.2023 - 127332 - RBL --<

ENDENHANCEMENT.
