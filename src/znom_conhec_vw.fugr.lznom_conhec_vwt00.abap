*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZNOM_CONHEC.....................................*
DATA:  BEGIN OF STATUS_ZNOM_CONHEC                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNOM_CONHEC                   .
CONTROLS: TCTRL_ZNOM_CONHEC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZNOM_CONHEC                   .
TABLES: ZNOM_CONHEC                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
