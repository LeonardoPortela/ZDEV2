*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFI0092
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFI0092            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
