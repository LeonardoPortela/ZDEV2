*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0076........................................*
DATA:  BEGIN OF STATUS_ZSDT0076                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0076                      .
CONTROLS: TCTRL_ZSDT0076
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0076                      .
TABLES: ZSDT0076                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
