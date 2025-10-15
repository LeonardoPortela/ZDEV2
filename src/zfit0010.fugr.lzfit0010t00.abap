*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0010........................................*
DATA:  BEGIN OF STATUS_ZFIT0010                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0010                      .
CONTROLS: TCTRL_ZFIT0010
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0010                      .
TABLES: ZFIT0010                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
