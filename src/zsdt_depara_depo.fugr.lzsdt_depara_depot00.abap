*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT_DEPARA_DEPO................................*
DATA:  BEGIN OF STATUS_ZSDT_DEPARA_DEPO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT_DEPARA_DEPO              .
CONTROLS: TCTRL_ZSDT_DEPARA_DEPO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT_DEPARA_DEPO              .
TABLES: ZSDT_DEPARA_DEPO               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
