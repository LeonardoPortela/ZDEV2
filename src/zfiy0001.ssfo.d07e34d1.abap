CONDENSE v_imp_ret_txt  NO-GAPS.

WRITE: st_pago_ret-wt_qbshh TO v_imp_ret_txt.

CONDENSE v_imp_ret_txt  NO-GAPS.

v_imp_qbshh = st_pago_ret-wt_qbshh + v_imp_qbshh.

CLEAR: v_tot_ret.

WRITE: v_imp_qbshh TO v_tot_ret.

CONDENSE v_tot_ret  NO-GAPS.
















