*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0028........................................*
DATA:  BEGIN OF STATUS_ZFIT0028                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0028                      .
CONTROLS: TCTRL_ZFIT0028
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0028                      .
TABLES: ZFIT0028                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
