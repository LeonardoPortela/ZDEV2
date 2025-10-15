"Name: \PR:SAPMV50A\FO:USEREXIT_DELETE_DOCUMENT\SE:BEGIN\EI
ENHANCEMENT 0 Z_APAGA_LIMORDEM.
*
  data VREFER      TYPE ZSDT0151-CH_REFERENCIA.
  If sy-tcode+0(2) = 'VL'.
      clear VREFER.
  else.
      IMPORT  VREFER  FROM MEMORY ID 'MREFER'.
  endif.
  if VREFER is INITIAL.
   DELETE
       FROM ZSDT0151
       WHERE VKORG          = LIKP-VKORG
       AND   WERKS          = LIPS-WERKS
       AND   VBELN          = LIPS-VGBEL
       AND   STATUS         = ''.
  else.
   DELETE
       FROM ZSDT0151
       WHERE VKORG          = LIKP-VKORG
       AND   WERKS          = LIPS-WERKS
       AND   VBELN          = LIPS-VGBEL
       AND   STATUS         = ''
       AND   CH_REFERENCIA  = VREFER.
  endif.

ENDENHANCEMENT.
