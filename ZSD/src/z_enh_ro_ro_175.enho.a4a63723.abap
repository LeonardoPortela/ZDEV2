"Name: \TY:CL_TAX_CALC_BR_SD\ME:CALC_ICMS_VALUE\SE:END\EI
ENHANCEMENT 0 Z_ENH_RO_RO_175.
*"Para cenário RO-RO, nota sap: 3010156
IF MS_TAX_DATA-ICMSRATE = '0.175000' AND  MS_TAX_DATA-ICMSBASE = '0.057100'.

  IF mv_icms_incl = ''.
    CALL METHOD super->calc_icms_value
      EXPORTING
        iv_val_incl_icms     = iv_val_incl_icms
        iv_val_incl_icms_ipi = iv_val_incl_icms_ipi
      IMPORTING
        ev_icms              = ev_icms
        ev_dsc100            = ev_dsc100.
  ELSE.

    IF check_icms_active( ) <> 'X'.
      ev_icms = 0.
      RETURN.
    ELSE.

* Conhecimento : Calculation using ICMS freight rate       "1677119
      IF check_cte_active( ) = 'X'.                        "1782182
        ms_tax_result-conh_icm_amt =                       "1677119
          iv_val_incl_icms * ms_tax_data-icmsfreightrate   "1677119
                           * ms_tax_data-icmsbase.         "1714749
        RETURN.                                            "1677119
      ENDIF.                                               "1677119

*Determine Calculation Base depending on usage
      IF ms_tax_control-usage = mc_indust.
* Industrialization: Calculation Base is amount including ICMS
        lv_eff_icms_base = iv_val_incl_icms.
      ELSE.
* Industrialization: Calculation Base is amount including ICMS and IPI
        lv_eff_icms_base = iv_val_incl_icms_ipi.
      ENDIF.

"Ajuste Nota 3010156 Excluir calculo
      " lv_red_icms_rate = ms_tax_data-icmsrate * ms_tax_data-icmsbase.
*
* Reduced base amount to calculate ICMS is required
     mv_icms_reduced_base = lv_eff_icms_base * ms_tax_data-icmsbase.
     ev_icms = lv_eff_icms_base * ms_tax_data-icmsrate * ms_tax_data-icmsbase. "Novo calculo
* Adjust for Convênio 100
      IF ms_tax_data-icmsconv100 = 'X'.
        ev_dsc100 = lv_eff_icms_base * ms_tax_data-icmsrate - ev_icms.
*       In ICMS FCP scenarios, the rounding routine will be executed later    "2443042
        IF ms_tax_control-icms_fcp = abap_false.                              "2443042
          CALL METHOD round_value                                             "2443042 1562248
            CHANGING                                                          "2443042 1562248
              cv_value = ev_dsc100.                                           "2443042 1562248
        ENDIF.                                                                "2443042
      ELSE.
        ev_dsc100 = 0.
      ENDIF.
    ENDIF.

  ENDIF.


   BREAK LPORTELA.


  ENDIF.




ENDENHANCEMENT.
