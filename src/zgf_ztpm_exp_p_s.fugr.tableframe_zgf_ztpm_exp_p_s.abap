*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGF_ZTPM_EXP_P_S
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGF_ZTPM_EXP_P_S   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
