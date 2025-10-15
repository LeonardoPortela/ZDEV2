*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT_DEPARA_CENV................................*
DATA:  BEGIN OF STATUS_ZSDT_DEPARA_CENV              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT_DEPARA_CENV              .
CONTROLS: TCTRL_ZSDT_DEPARA_CENV
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT_DEPARA_CENV              .
TABLES: ZSDT_DEPARA_CENV               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
