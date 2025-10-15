*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMR0004........................................*
DATA:  BEGIN OF STATUS_ZPMR0004                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMR0004                      .
CONTROLS: TCTRL_ZPMR0004
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZPMR0004                      .
TABLES: ZPMR0004                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
