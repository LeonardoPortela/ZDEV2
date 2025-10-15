*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0120........................................*
DATA:  BEGIN OF STATUS_ZFIT0120                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0120                      .
CONTROLS: TCTRL_ZFIT0120
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0120                      .
TABLES: ZFIT0120                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
