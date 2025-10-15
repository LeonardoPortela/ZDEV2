*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0068........................................*
DATA:  BEGIN OF STATUS_ZFIT0068                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0068                      .
CONTROLS: TCTRL_ZFIT0068
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0068                      .
TABLES: ZFIT0068                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
