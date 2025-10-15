*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0121........................................*
DATA:  BEGIN OF STATUS_ZSDT0121                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0121                      .
CONTROLS: TCTRL_ZSDT0121
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0121                      .
TABLES: ZSDT0121                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
