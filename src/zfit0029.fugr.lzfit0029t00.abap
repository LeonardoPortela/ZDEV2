*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0029........................................*
DATA:  BEGIN OF STATUS_ZFIT0029                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0029                      .
CONTROLS: TCTRL_ZFIT0029
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0029                      .
TABLES: ZFIT0029                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
