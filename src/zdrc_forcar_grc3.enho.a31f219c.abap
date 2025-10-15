"Name: \TY:CL_J_1BNFE_CUST4_DA\ME:SEARCH_CLOUD_SKIP_SERVICE\SE:BEGIN\EI
ENHANCEMENT 0 ZDRC_FORCAR_GRC3.
*
*DRC-#127423-13.11.2023-JT-inicio
    DATA: l_envia_grc    TYPE char1.

    l_envia_grc = valida_envio_grc( is_nf ).

    IF l_envia_grc = abap_true.
      EXIT.
    ENDIF.
*DRC-#127423-13.11.2023-JT-fim
*
ENDENHANCEMENT.
