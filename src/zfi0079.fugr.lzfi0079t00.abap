*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0095........................................*
DATA:  BEGIN OF STATUS_ZFIT0095                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0095                      .
CONTROLS: TCTRL_ZFIT0095
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0095                      .
TABLES: ZFIT0095                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
