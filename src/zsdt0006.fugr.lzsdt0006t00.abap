*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0006........................................*
DATA:  BEGIN OF STATUS_ZSDT0006                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0006                      .
CONTROLS: TCTRL_ZSDT0006
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0006                      .
TABLES: ZSDT0006                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
