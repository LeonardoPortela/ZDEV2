"Name: \TY:CL_EDOC_BR_CNPJ_FM_WRAP\IN:IF_EDOC_BR_CNPJ_FM_WRAP\ME:GET_ORGDATA_FROM_CNPJ\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_GET_ORGDATA_FROM_CNPJ.

  IF zcl_nfe_aux=>verificar_cnpj_terceiro( iv_cnpj ) = abap_true.

    rt_orgdata = zcl_nfe_aux=>get_orgdata_terceiro( iv_cnpj ).

    IF rt_orgdata IS NOT INITIAL.
      RETURN.
    ENDIF.

  ENDIF.

ENDENHANCEMENT.
