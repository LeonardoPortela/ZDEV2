*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSE01_APROPR_SEG
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSE01_APROPR_SEG   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
