*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZCIOT_PARCEIROS
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZCIOT_PARCEIROS    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
