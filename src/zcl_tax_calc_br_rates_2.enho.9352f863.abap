"Name: \TY:CL_TAX_CALC_BR_MM\ME:DETERMINE_ADJUSTED_NET\SE:END\EI
ENHANCEMENT 0 ZCL_TAX_CALC_BR_RATES_2.
*

  DATA: VL_ITEM TYPE BAPIWRBTR.
  DATA: VL_dif  TYPE BAPIWRBTR.
  DATA: E_CHECK TYPE CHAR01.

  ZCL_TAXA=>GET_CK_PIS_COFINS(
    EXPORTING
      I_MWSKZ = ME->MS_KOMK-MWSKZ " Código do IVA
    CHANGING
      E_CHECK = E_CHECK    " Alíquota
  ).

  IF E_CHECK EQ ABAP_TRUE.
    IMPORT P1 = VL_ITEM FROM MEMORY ID 'MZMMR019VLRITEM'.
    VL_dif = abs( VL_ITEM - mv_adjusted_net ).
    VL_dif = VL_dif * 100.
    if VL_dif le 3.
      mv_adjusted_net = VL_ITEM.
    ELSEIF VL_ITEM GT 1 AND mv_adjusted_net LT 1.
      mv_adjusted_net = 0.
    endif.
  ENDIF.


ENDENHANCEMENT.
