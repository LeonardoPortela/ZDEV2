"Name: \TY:CL_EDOC_BR_PROTOCOL\ME:CHECK_STATUS\SE:BEGIN\EI
ENHANCEMENT 0 ZDRC_CHECK_AUTORIZA2.
*
*DRC-#127012-10.11.2023-JT-inicio
    rs_result = avalia_check_autorizacao( iv_is_nfe = iv_is_nfe
                                          iv_xml    = iv_xml ).
    IF rs_result IS NOT INITIAL.
      EXIT.
    ENDIF.
*DRC-#127012-10.11.2023-JT-fim
*
ENDENHANCEMENT.
