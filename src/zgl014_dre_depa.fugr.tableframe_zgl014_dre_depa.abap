*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGL014_DRE_DEPA
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGL014_DRE_DEPA    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
