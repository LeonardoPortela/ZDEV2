*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMR0002........................................*
DATA:  BEGIN OF STATUS_ZPMR0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMR0002                      .
CONTROLS: TCTRL_ZPMR0002
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZPMR0002                      .
TABLES: ZPMR0002                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
