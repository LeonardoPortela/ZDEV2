*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0122........................................*
DATA:  BEGIN OF STATUS_ZSDT0122                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0122                      .
CONTROLS: TCTRL_ZSDT0122
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0122                      .
TABLES: ZSDT0122                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
