*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMMF0039
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMMF0039           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
