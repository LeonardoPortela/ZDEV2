METHOD should_sign_qrcode .

  rv_result = cl_nfe_cloud_constant_boolean=>false.

  IF iv_issuing_type = cl_nfe_cloud_cte_enum_issuing=>epec OR iv_issuing_type = cl_nfe_cloud_cte_enum_issuing=>fsda.
    rv_result = cl_nfe_cloud_constant_boolean=>true.
  ENDIF.

ENDMETHOD.
