*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0079........................................*
DATA:  BEGIN OF STATUS_ZSDT0079                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0079                      .
CONTROLS: TCTRL_ZSDT0079
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0079                      .
TABLES: ZSDT0079                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
