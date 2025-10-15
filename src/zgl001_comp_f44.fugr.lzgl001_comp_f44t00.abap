*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0006........................................*
DATA:  BEGIN OF STATUS_ZFIT0006                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0006                      .
CONTROLS: TCTRL_ZFIT0006
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZFIT0006                      .
TABLES: ZFIT0006                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
