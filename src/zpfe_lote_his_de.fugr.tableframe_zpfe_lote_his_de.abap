*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZPFE_LOTE_HIS_DE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZPFE_LOTE_HIS_DE   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
