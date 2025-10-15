*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0040........................................*
DATA:  BEGIN OF STATUS_ZFIT0040                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0040                      .
CONTROLS: TCTRL_ZFIT0040
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0040                      .
TABLES: ZFIT0040                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
