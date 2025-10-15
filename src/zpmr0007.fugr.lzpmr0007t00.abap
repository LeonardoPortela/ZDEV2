*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMR0007........................................*
DATA:  BEGIN OF STATUS_ZPMR0007                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMR0007                      .
CONTROLS: TCTRL_ZPMR0007
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZPMR0007                      .
TABLES: ZPMR0007                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
