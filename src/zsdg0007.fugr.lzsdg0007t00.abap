*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT_EX_ICMS_DES................................*
DATA:  BEGIN OF STATUS_ZMMT_EX_ICMS_DES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT_EX_ICMS_DES              .
CONTROLS: TCTRL_ZMMT_EX_ICMS_DES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT_EX_ICMS_DES              .
TABLES: ZMMT_EX_ICMS_DES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
