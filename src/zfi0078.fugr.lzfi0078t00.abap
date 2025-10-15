*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0094........................................*
DATA:  BEGIN OF STATUS_ZFIT0094                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0094                      .
CONTROLS: TCTRL_ZFIT0094
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0094                      .
TABLES: ZFIT0094                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
