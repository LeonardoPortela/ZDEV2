*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0103........................................*
DATA:  BEGIN OF STATUS_ZFIT0103                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0103                      .
CONTROLS: TCTRL_ZFIT0103
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0103                      .
TABLES: ZFIT0103                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
