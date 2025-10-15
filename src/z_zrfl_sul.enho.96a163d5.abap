"Name: \PR:SAPLV60A\FO:XVBRK_BEARBEITEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_ZRFL_SUL.
*
  if VBRK-FKART = 'ZRFL' or VBRK-FKART = 'ZRDC'.
      data Vlines  type  sy-tabix.
      IF VBRK-valdt = ' '.
          VBRK-valdt = '00000000'.
      ENDIF.
      Vlines = lines( XVBUP ).
      IF vlines gt 1.
         vbrk-inco1 = ovbrk-fix-inco1.
         vbrk-inco2 = ovbrk-fix-inco2.
      ENDIF.
  Endif.

ENDENHANCEMENT.
