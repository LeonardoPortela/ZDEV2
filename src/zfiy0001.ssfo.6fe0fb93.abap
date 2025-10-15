
WRITE v_wrbtr_t    TO v_wrbtr_txt . " IMP Dolar
WRITE v_dmbtr_t    TO v_totalc_txt. " CANCELACIONES
WRITE v_wt_qbshh_t TO v_totalr_txt. " RETENCIONES
WRITE v_imp_neto_t TO v_totalt_txt. " TOTAL

CONDENSE v_wrbtr_txt  NO-GAPS.
CONDENSE v_totalc_txt NO-GAPS.
CONDENSE v_totalr_txt NO-GAPS.
CONDENSE v_totalt_txt NO-GAPS.


















