*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZNOM_TRANSPORTE.................................*
DATA:  BEGIN OF STATUS_ZNOM_TRANSPORTE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNOM_TRANSPORTE               .
CONTROLS: TCTRL_ZNOM_TRANSPORTE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZNOM_TRANSPORTE               .
TABLES: ZNOM_TRANSPORTE                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
