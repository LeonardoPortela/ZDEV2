*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZCARGA_CTE_TIP
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZCARGA_CTE_TIP     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
