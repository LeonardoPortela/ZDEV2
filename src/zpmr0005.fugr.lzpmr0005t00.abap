*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMR0005........................................*
DATA:  BEGIN OF STATUS_ZPMR0005                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMR0005                      .
CONTROLS: TCTRL_ZPMR0005
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZPMR0005                      .
TABLES: ZPMR0005                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
