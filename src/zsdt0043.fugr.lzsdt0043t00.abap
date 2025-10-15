*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0043........................................*
DATA:  BEGIN OF STATUS_ZSDT0043                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0043                      .
CONTROLS: TCTRL_ZSDT0043
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0043                      .
TABLES: ZSDT0043                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
