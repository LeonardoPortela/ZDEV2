*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0024........................................*
DATA:  BEGIN OF STATUS_ZSDT0024                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0024                      .
CONTROLS: TCTRL_ZSDT0024
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0024                      .
TABLES: ZSDT0024                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
