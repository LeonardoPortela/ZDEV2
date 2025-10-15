*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0050........................................*
DATA:  BEGIN OF STATUS_ZFIT0050                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0050                      .
CONTROLS: TCTRL_ZFIT0050
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0050                      .
TABLES: ZFIT0050                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
