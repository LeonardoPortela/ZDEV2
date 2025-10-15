*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCONTAS_SPED....................................*
DATA:  BEGIN OF STATUS_ZCONTAS_SPED                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCONTAS_SPED                  .
CONTROLS: TCTRL_ZCONTAS_SPED
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCONTAS_SPED                  .
TABLES: ZCONTAS_SPED                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
