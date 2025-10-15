*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0072........................................*
DATA:  BEGIN OF STATUS_ZFIT0072                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0072                      .
CONTROLS: TCTRL_ZFIT0072
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0072                      .
TABLES: ZFIT0072                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
