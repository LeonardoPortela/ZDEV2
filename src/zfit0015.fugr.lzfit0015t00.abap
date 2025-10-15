*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0014........................................*
DATA:  BEGIN OF STATUS_ZFIT0014                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0014                      .
CONTROLS: TCTRL_ZFIT0014
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFIT0015........................................*
DATA:  BEGIN OF STATUS_ZFIT0015                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0015                      .
CONTROLS: TCTRL_ZFIT0015
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZFIT0014                      .
TABLES: *ZFIT0015                      .
TABLES: ZFIT0014                       .
TABLES: ZFIT0015                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
