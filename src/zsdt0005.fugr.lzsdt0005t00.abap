*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0005........................................*
DATA:  BEGIN OF STATUS_ZSDT0005                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0005                      .
CONTROLS: TCTRL_ZSDT0005
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0005                      .
TABLES: ZSDT0005                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
