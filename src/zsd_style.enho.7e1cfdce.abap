"Name: \PR:SAPLSLVC\FO:LAYOUT_KKBLO_LVC\SE:END\EI
ENHANCEMENT 0 ZSD_STYLE.
*
*-Ajuste style - jtassoni - 18.09.2020 - inicio
  IF rs_layout_kkblo-expand_fieldname IS NOT INITIAL.
    rs_layout_lvc-stylefname = rs_layout_kkblo-expand_fieldname.
  ENDIF.
*-Ajuste style - jtassoni - 18.09.2020 - fim
*
ENDENHANCEMENT.
