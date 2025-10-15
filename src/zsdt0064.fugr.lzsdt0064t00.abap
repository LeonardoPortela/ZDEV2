*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0064........................................*
DATA:  BEGIN OF STATUS_ZSDT0064                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0064                      .
CONTROLS: TCTRL_ZSDT0064
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0064                      .
TABLES: ZSDT0064                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
