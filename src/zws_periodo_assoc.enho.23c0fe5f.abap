"Name: \PR:SAPLJ1AE\FO:DATA_RETRIEVAL_SD_2485\SE:END\EI
ENHANCEMENT 0 ZWS_PERIODO_ASSOC.
*ENHANCEMENT Provisório até a nota SAP 3352701 estar liberada, no fluxo standard não está entrando na badi implementada.
   CLEAR : wa_error_flag.
  PERFORM is_credit_debit CHANGING lv_is_credit_debit.              "2938521

  IF lv_is_credit_debit = 'X'.                                      "2935576

     " IF lv_badi_2485 IS NOT INITIAL.                                 "3261370
        TRY.                                                          "3261370
          GET BADI lv_badi_2485.                                      "3261370
        CALL BADI lv_badi_2485->change_per_asoc                       "2935576
          EXPORTING                                                   "2935576
            is_cae_det  = wa_caedet                                   "2935576
            is_j_1acae  = w_j_1acae                                   "2958749
          IMPORTING                                                   "2935576
            es_per_asoc = ls_per_asoc.                                "2935576
          CATCH cx_badi_not_implemented INTO lx_badi_not_implemented. "3261370
        ENDTRY.                                                       "3261370
    "  ENDIF.                                                          "3113739

      PERFORM validate_per_asoc USING ls_per_asoc.                    "2935576

"">>>>>Provisório somente para aproveitar o mapeamento Z no PI  já existende, após migração ao CPI utilizar estruturas standard.
         WA_CAEDET-periodo_asoc-fchdesde = ls_per_asoc-fchdesde .
         WA_CAEDET-periodo_asoc-fchhasta  = ls_per_asoc-fchhasta.
***<<<<<
    ENDIF.                                                            "2935576

**End of Note 1653495



ENDENHANCEMENT.
