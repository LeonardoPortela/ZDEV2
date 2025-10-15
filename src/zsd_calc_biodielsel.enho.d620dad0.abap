"Name: \PR:SAPLV61A\FO:CONDITION_UPDATE_KAWRT\SE:END\EI
ENHANCEMENT 0 ZSD_CALC_BIODIELSEL.
*
*US #124643 - 02.10.2023 - JT - inicio
 IF xkomv-kofra NE 0 AND basisformel = space AND komp-kposn NE 0.
   IF xkomv-kofra LE 999.
     PERFORM frm_kond_basis_904 IN PROGRAM saplv61a IF FOUND.
   ENDIF.
 ENDIF.
*
 IF xkomv-kofrm NE 0 AND wertformel EQ space AND komp-kposn NE 0.
   IF xkomv-kofrm LE 999.
     PERFORM frm_kondi_wert_911 IN PROGRAM saplv61a IF FOUND.
   ENDIF.
 ENDIF.
*US #124643 - 02.10.2023 - JT - fim
*
ENDENHANCEMENT.
