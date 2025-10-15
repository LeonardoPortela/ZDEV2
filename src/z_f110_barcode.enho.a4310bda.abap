"Name: \PR:RFFOBR_U\FO:SORTBOLETO\SE:BEGIN\EI
ENHANCEMENT 0 Z_F110_BARCODE.
*
   DATA ls_bseg2 TYPE bseg.
   SELECT SINGLE * FROM bseg
       INTO ls_bseg2
           WHERE bukrs EQ regup-bukrs
             AND belnr EQ regup-belnr
             AND gjahr EQ regup-gjahr
             AND buzei EQ regup-buzei.
   IF ls_bseg2-glo_ref1 IS INITIAL AND ls_bseg2-esrre IS NOT INITIAL.
     regup-esrre = ls_bseg2-esrre.
     regup-esrnr = ls_bseg2-esrnr.
     regup-esrpz = ls_bseg2-esrpz.
   ENDIF.

ENDENHANCEMENT.
