*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0086........................................*
DATA:  BEGIN OF STATUS_ZFIT0086                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0086                      .
CONTROLS: TCTRL_ZFIT0086
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0086                      .
TABLES: ZFIT0086                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
