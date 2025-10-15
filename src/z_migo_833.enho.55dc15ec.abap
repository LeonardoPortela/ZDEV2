"Name: \PR:SAPMM07M\FO:M-SEGMENTE_GENERIEREN\SE:BEGIN\EI
ENHANCEMENT 0 Z_MIGO_833.
*
  LOOP AT zmseg  ASSIGNING FIELD-SYMBOL(<fs_mseg>).
      IF <fs_mseg>-bwart = '833'.
         clear  <fs_mseg>-UMLGO.
         clear  <fs_mseg>-UMCHA.
      ENDIF.
  ENDLOOP.
ENDENHANCEMENT.
