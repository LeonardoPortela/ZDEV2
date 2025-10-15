*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZRSI_ICMS_MONO
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZRSI_ICMS_MONO     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
