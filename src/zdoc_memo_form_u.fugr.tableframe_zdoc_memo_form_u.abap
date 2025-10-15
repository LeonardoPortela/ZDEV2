*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDOC_MEMO_FORM_U
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDOC_MEMO_FORM_U   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
