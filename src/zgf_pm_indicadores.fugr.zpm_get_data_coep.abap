FUNCTION ZPM_GET_DATA_COEP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_START) TYPE  DATS
*"     VALUE(I_END) TYPE  DATS
*"  TABLES
*"      COEP STRUCTURE  COEP
*"----------------------------------------------------------------------

CALL FUNCTION 'ZPM_GET_DATA_COBK'
 EXPORTING
   I_START       = I_START
   I_END         = I_END
  TABLES
    cobk          = t_cobk.


IF t_cobk IS NOT INITIAL.
  SELECT * FROM COEP INTO TABLE coep
    FOR ALL ENTRIES IN t_cobk
    WHERE BELNR = t_cobk-belnr
      AND KOKRS = t_cobk-KOKRS.
ENDIF.

ENDFUNCTION.
