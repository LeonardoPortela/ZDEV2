*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSDT0076
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSDT0076           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
