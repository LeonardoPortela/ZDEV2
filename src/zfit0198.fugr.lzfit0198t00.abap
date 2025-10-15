*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0198........................................*
DATA:  BEGIN OF STATUS_ZFIT0198                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0198                      .
CONTROLS: TCTRL_ZFIT0198
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFIT0199........................................*
DATA:  BEGIN OF STATUS_ZFIT0199                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0199                      .
CONTROLS: TCTRL_ZFIT0199
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZFIT0198                      .
TABLES: *ZFIT0199                      .
TABLES: ZFIT0198                       .
TABLES: ZFIT0199                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
