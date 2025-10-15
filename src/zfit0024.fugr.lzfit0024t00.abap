*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0024........................................*
DATA:  BEGIN OF STATUS_ZFIT0024                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0024                      .
CONTROLS: TCTRL_ZFIT0024
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0024                      .
TABLES: ZFIT0024                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
