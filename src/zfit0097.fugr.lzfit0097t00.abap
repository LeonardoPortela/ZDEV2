*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0097........................................*
DATA:  BEGIN OF STATUS_ZFIT0097                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0097                      .
CONTROLS: TCTRL_ZFIT0097
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0097                      .
TABLES: ZFIT0097                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
