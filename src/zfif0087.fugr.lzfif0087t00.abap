*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0087........................................*
DATA:  BEGIN OF STATUS_ZFIT0087                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0087                      .
CONTROLS: TCTRL_ZFIT0087
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0087                      .
TABLES: ZFIT0087                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
