*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMP_CONTAS_CONS................................*
DATA:  BEGIN OF STATUS_ZIMP_CONTAS_CONS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMP_CONTAS_CONS              .
CONTROLS: TCTRL_ZIMP_CONTAS_CONS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMP_CONTAS_CONS              .
TABLES: ZIMP_CONTAS_CONS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
