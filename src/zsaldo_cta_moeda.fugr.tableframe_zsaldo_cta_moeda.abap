*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSALDO_CTA_MOEDA
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSALDO_CTA_MOEDA   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
