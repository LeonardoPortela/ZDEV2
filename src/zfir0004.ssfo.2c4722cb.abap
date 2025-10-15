CLEAR: WG_PC_REEM.

IF WG_TOTAL LT '0'.
  WG_TOTAL = ( WG_TOTAL * ( -1 ) ).
ENDIF.
WG_PC_REEM = ( WG_TOTAL_NF - ( TOT_ADTO + WG_TOTAL ) ).

*WG_PC_IMP = ( WG_PC_IMP * ( -1 ) ).
*wg_pc_reem = ( ( tot_adto + wg_pc_imp ) - WG_TOT_NF  ).





*WG_PC_REEM = ( TOT_ADTO -  ( WG_TOT_NF + WG_PC_IMP ) ).











