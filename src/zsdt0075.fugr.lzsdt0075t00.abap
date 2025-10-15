*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0075........................................*
DATA:  BEGIN OF STATUS_ZSDT0075                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0075                      .
CONTROLS: TCTRL_ZSDT0075
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0075                      .
TABLES: ZSDT0075                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
