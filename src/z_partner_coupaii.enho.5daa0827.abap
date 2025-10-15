"Name: \PR:SAPL2012\FO:PROCESS_PO_PARTNER\SE:END\EI
ENHANCEMENT 0 Z_PARTNER_COUPAII.
*
  IF ls_header-bsart+0(1) = 'Y'.
     ls_headerx-lifre = 'X'.
     LOOP AT lt_mmpa.
         lt_mmpa-ebeln = ls_header-ebeln.
         MODIFY lt_mmpa INDEX sy-tabix.
     ENDLOOP.
  ENDIF.
ENDENHANCEMENT.
