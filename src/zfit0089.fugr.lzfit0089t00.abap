*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0089........................................*
DATA:  BEGIN OF STATUS_ZFIT0089                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0089                      .
CONTROLS: TCTRL_ZFIT0089
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0089                      .
TABLES: ZFIT0089                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
