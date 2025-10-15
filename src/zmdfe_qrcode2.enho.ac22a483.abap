"Name: \TY:CL_NFE_CLOUD_MDFE_PROCESSOR\ME:CONSTRUCTOR\SE:END\EI
ENHANCEMENT 0 ZMDFE_QRCODE2.
*
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    IF io_region_map IS BOUND.
      mo_region_map = io_region_map.
    ELSE.
      CREATE OBJECT mo_region_map TYPE cl_nfe_region_map.
    ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
*
ENDENHANCEMENT.
