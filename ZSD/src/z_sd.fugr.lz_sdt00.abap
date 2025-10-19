*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0001........................................*
DATA:  BEGIN OF STATUS_ZSDT0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001                      .
CONTROLS: TCTRL_ZSDT0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0001                      .
TABLES: ZSDT0001                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
