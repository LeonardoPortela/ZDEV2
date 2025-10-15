*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0091........................................*
DATA:  BEGIN OF STATUS_ZFIT0091                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0091                      .
CONTROLS: TCTRL_ZFIT0091
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0091                      .
TABLES: ZFIT0091                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
