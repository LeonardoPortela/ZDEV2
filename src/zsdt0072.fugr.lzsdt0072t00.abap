*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0072........................................*
DATA:  BEGIN OF STATUS_ZSDT0072                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0072                      .
CONTROLS: TCTRL_ZSDT0072
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0072                      .
TABLES: ZSDT0072                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
