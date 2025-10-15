"Name: \PR:J_1A_WS_MONITOR\FO:FILL_IT_FIELDCATALOG\SE:BEGIN\EI
ENHANCEMENT 0 ZPRINT_NFE_ARG.
*
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'J_1ACAE'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  wa_fieldcatalog-fieldname = 'ZPRINT_NFE_AGR'.
  wa_fieldcatalog-col_pos   = 1.
  wa_fieldcatalog-reptext   = 'Print'.
  wa_fieldcatalog-outputlen = 2.
  wa_fieldcatalog-just      = 'C'.
  wa_fieldcatalog-hotspot   = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

** Add field for Status field
  wa_fieldcatalog-fieldname = 'STATUS'.
  wa_fieldcatalog-col_pos   = 2.
  wa_fieldcatalog-reptext   = text-029.
  wa_fieldcatalog-just      = 'C'.
  wa_fieldcatalog-outputlen = 5.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

** Add field for Log field
  wa_fieldcatalog-fieldname = 'LOG'.
  wa_fieldcatalog-col_pos   = 3.
  wa_fieldcatalog-reptext   = text-030.
  wa_fieldcatalog-outputlen = 2.
  wa_fieldcatalog-just      = 'C'.
  wa_fieldcatalog-hotspot   = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

** Add field for Log field
  wa_fieldcatalog-fieldname = 'ZRFC_UNLOCK'.
  wa_fieldcatalog-reptext   = 'Unlock'.
  clear: wa_fieldcatalog-col_pos.
  wa_fieldcatalog-outputlen = 2.
  wa_fieldcatalog-just      = 'C'.
  wa_fieldcatalog-hotspot   = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

** sort based on col_pos
  SORT it_fieldcatalog BY col_pos.

  LOOP AT it_fieldcatalog INTO wa_fieldcatalog. " Modify field catalog
**   Define column position
    CASE wa_fieldcatalog-fieldname.
      WHEN 'ZPRINT_NFE_AGR'.
        wa_fieldcatalog-col_pos   = 1.
      WHEN 'STATUS'.
        wa_fieldcatalog-col_pos   = 2.
      WHEN 'LOG'.
        wa_fieldcatalog-col_pos   = 3.
      WHEN OTHERS.
        wa_fieldcatalog-col_pos   = wa_fieldcatalog-col_pos + 2.
    ENDCASE.

    CASE wa_fieldcatalog-fieldname.
      WHEN 'CAE_REF'.
        wa_fieldcatalog-hotspot = 'X'.
    ENDCASE.

    MODIFY it_fieldcatalog FROM wa_fieldcatalog INDEX sy-tabix.
    CLEAR wa_fieldcatalog.
  ENDLOOP.

  CHECK 1 = 2.

ENDENHANCEMENT.
