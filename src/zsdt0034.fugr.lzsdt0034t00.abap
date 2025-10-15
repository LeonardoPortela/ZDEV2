*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0034........................................*
DATA:  BEGIN OF STATUS_ZSDT0034                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0034                      .
CONTROLS: TCTRL_ZSDT0034
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0034                      .
TABLES: ZSDT0034                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
