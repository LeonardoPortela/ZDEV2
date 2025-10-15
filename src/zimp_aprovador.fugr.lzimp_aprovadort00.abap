*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMP_APROVADOR..................................*
DATA:  BEGIN OF STATUS_ZIMP_APROVADOR                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMP_APROVADOR                .
CONTROLS: TCTRL_ZIMP_APROVADOR
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZIMP_APROVADOR                .
TABLES: ZIMP_APROVADOR                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
