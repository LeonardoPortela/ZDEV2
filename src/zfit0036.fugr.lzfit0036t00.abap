*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0036........................................*
DATA:  BEGIN OF STATUS_ZFIT0036                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0036                      .
CONTROLS: TCTRL_ZFIT0036
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0036                      .
TABLES: ZFIT0036                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
