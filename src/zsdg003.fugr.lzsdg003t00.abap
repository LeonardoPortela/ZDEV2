*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0007........................................*
DATA:  BEGIN OF STATUS_ZSDT0007                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0007                      .
CONTROLS: TCTRL_ZSDT0007
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0007                      .
TABLES: ZSDT0007                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
