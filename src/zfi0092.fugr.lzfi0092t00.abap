*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0098........................................*
DATA:  BEGIN OF STATUS_ZFIT0098                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0098                      .
CONTROLS: TCTRL_ZFIT0098
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0098                      .
TABLES: ZFIT0098                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
