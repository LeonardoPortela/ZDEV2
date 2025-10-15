*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZIB_NFE_DIST_TVO
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZIB_NFE_DIST_TVO   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
