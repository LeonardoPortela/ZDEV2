*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGL001_COMP_F44
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGL001_COMP_F44    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
