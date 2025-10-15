*data: vl_WRBTR  type char50.


*case j_1ai02-WAERS .
*when 'ARS'.
*  move '$'   to V_MONEDA.
*when 'USD'.
*  move 'U$S' to V_MONEDA.
*endcase.

MOVE '$'   TO v_moneda.

*write j_1ai02-WRBTR to vl_WRBTR.
*
*concatenate Vl_MONEDA vl_WRBTR
*into V_IMP_INFO separated by space.
*break jmona.


















