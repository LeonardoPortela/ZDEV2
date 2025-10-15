*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0134........................................*
DATA:  BEGIN OF STATUS_ZFIT0134                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0134                      .
CONTROLS: TCTRL_ZFIT0134
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0134                      .
TABLES: ZFIT0134                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
