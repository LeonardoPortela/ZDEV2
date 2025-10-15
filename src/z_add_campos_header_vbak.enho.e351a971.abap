"Name: \TY:CL_IM_SD_SEPA_BAPI_INTERFACE\IN:IF_EX_BADI_SD_SEPA_BAPI\ME:SD_SEPA_BAPI_001\SE:END\EI
ENHANCEMENT 0 Z_ADD_CAMPOS_HEADER_VBAK.
*

    if is_order_header_in is supplied and cs_vbakkom is supplied.
      MOVE is_order_header_in-ZTROCANOTA TO cs_vbakkom-ZTROCANOTA.
    endif.

    if is_order_header_inx is supplied and cs_vbakkomx is supplied.
      MOVE is_order_header_inx-ZTROCANOTA TO cs_vbakkomx-ZTROCANOTA.
    endif.

ENDENHANCEMENT.
