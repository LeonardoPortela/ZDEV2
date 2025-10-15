*DATA QBSHB TYPE QBSHB.
*CLEAR VL_WRBTR.
*if J_1AI02-QBSHB < 0.
*  QBSHB = J_1AI02-QBSHB * - 1 .
*ELSE.
*  QBSHB = J_1AI02-QBSHB .
*ENDIF.

*vl_WRBTR = J_1AI02-WRBTR
*         + QBSHB.


write  VL_wrbtr    to V_wrbtr  RIGHT-JUSTIFIED.
Condense V_wrbtr   no-gaps.















