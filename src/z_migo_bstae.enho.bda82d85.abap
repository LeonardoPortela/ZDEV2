"Name: \PR:SAPLEINR\FO:XEBEFU_AUFBAUEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_MIGO_BSTAE.
*
  IF ( ( sy-cprog = 'ZMMR019' or sy-cprog = 'ZMMR020') and sy-tcode+0(3) ne 'VL3' ) or
     ( sy-tcode = 'MIGO' and SY-TITLE+0(7) = 'Estorno' ).
    LOOP AT pot.
       IF pot-bstae NE space.
          clear pot-bstae.
          MODIFY pot index sy-tabix TRANSPORTING bstae.
       endif.
    ENDLOOP.

  ENDIF.
ENDENHANCEMENT.
