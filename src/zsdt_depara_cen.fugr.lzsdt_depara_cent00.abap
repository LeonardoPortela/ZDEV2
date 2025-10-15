*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT_DEPARA_CEN.................................*
DATA:  BEGIN OF STATUS_ZSDT_DEPARA_CEN               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT_DEPARA_CEN               .
CONTROLS: TCTRL_ZSDT_DEPARA_CEN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT_DEPARA_CEN               .
TABLES: ZSDT_DEPARA_CEN                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
