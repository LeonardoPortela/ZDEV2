*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0025........................................*
DATA:  BEGIN OF STATUS_ZFIT0025                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0025                      .
CONTROLS: TCTRL_ZFIT0025
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0025                      .
TABLES: ZFIT0025                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
