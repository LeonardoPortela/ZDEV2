*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTPM_EXP_P_SAAF.................................*
DATA:  BEGIN OF STATUS_ZTPM_EXP_P_SAAF               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTPM_EXP_P_SAAF               .
CONTROLS: TCTRL_ZTPM_EXP_P_SAAF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTPM_EXP_P_SAAF               .
TABLES: ZTPM_EXP_P_SAAF                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
