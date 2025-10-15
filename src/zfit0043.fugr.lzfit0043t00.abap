*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0043........................................*
DATA:  BEGIN OF STATUS_ZFIT0043                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0043                      .
CONTROLS: TCTRL_ZFIT0043
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0043                      .
TABLES: ZFIT0043                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
