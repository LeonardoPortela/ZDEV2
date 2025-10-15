*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0043........................................*
DATA:  BEGIN OF STATUS_ZMMT0043                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0043                      .
CONTROLS: TCTRL_ZMMT0043
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0043                      .
TABLES: ZMMT0043                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
