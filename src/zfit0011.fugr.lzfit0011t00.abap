*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0011........................................*
DATA:  BEGIN OF STATUS_ZFIT0011                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0011                      .
CONTROLS: TCTRL_ZFIT0011
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0011                      .
TABLES: ZFIT0011                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
