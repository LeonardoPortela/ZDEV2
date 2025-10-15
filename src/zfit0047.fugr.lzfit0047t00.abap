*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0047........................................*
DATA:  BEGIN OF STATUS_ZFIT0047                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0047                      .
CONTROLS: TCTRL_ZFIT0047
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0047                      .
TABLES: ZFIT0047                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
