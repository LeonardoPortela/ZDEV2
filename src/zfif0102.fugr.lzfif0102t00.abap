*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0102........................................*
DATA:  BEGIN OF STATUS_ZFIT0102                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0102                      .
CONTROLS: TCTRL_ZFIT0102
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0102                      .
TABLES: ZFIT0102                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
