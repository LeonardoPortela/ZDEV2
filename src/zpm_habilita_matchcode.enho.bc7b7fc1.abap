"Name: \FU:FIELD_SELECTION_MODIFY_ALL\SE:END\EI
ENHANCEMENT 0 ZPM_HABILITA_MATCHCODE.
data is_equi type equi.

if sy-tcode = 'IE01' or
   sy-tcode = 'IE02' or
   sy-tcode = 'IE03' .

*IMPORT is_equi-eqtyp FROM MEMORY ID 'ZIE'.
*
*if is_equi-eqtyp = 'V' or is_equi-eqtyp = 'T'.

loop at screen.
  if SCREEN-name = 'ITOB-HERST' or SCREEN-name = 'ITOB-TYPBZ'.
    screen-value_help = 1.
    modify screen.
  endif.
ENDLOOP.

*endif.

endif.

ENDENHANCEMENT.
