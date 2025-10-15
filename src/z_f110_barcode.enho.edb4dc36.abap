"Name: \PR:RFFOBR_U\FO:GET_BARCODE\SE:BEGIN\EI
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
    IF par_brla = '240' OR par_fbla IS NOT INITIAL OR par_ver IS NOT INITIAL. "2536140
      IF ls_bseg2-esrnr(3) = reguh-ubnkl(3).
        payment_form = '30'.
      ELSE.
        payment_form = '31'.
      ENDIF.
    ELSE.
      IF reguh-ubnkl(3) = '237'.
        payment_form = '31'.
      ENDIF.
    ENDIF.
  ENDIF.


ENDENHANCEMENT.
