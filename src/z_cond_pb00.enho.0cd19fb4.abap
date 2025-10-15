"Name: \FU:SD_COND_ACCESS\SE:END\EI
ENHANCEMENT 0 Z_COND_PB00.
*
*  FIELD-SYMBOLS:  <kschl> TYPE kschl.
*
*  if CONDITION_TYPE = 'PB00'.
*     ASSIGN COMPONENT 'KSCHL' OF STRUCTURE condition_records TO <kschl>.
*     CHECK sy-subrc = 0.
*     LOOP AT condition_records.
*         IF <kschl> = 'PB00'.
*           DELETE condition_records.
*           EXIT.
*         ENDIF.
*     ENDLOOP.
*  endif.

ENDENHANCEMENT.
