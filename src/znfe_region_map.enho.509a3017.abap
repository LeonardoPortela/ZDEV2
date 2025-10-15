"Name: \TY:CL_NFE_REGION_MAP\ME:UFCODE_TO_REGION\SE:BEGIN\EI
ENHANCEMENT 0 ZNFE_REGION_MAP.

  zcl_estado=>zif_estado~get_instance( )->get_sigla_estado(
    EXPORTING
      i_id_bacen  = CONV #( iv_uf_code )
     IMPORTING
       e_uf       =  DATA(_SIGLA_UF)
    ).

  rv_result = _SIGLA_UF.

  CHECK _SIGLA_UF IS INITIAL.

ENDENHANCEMENT.
