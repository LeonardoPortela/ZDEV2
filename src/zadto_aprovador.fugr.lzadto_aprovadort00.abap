*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZADTO_APROVADOR.................................*
DATA:  BEGIN OF STATUS_ZADTO_APROVADOR               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZADTO_APROVADOR               .
CONTROLS: TCTRL_ZADTO_APROVADOR
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZADTO_APROVADOR               .
TABLES: ZADTO_APROVADOR                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
