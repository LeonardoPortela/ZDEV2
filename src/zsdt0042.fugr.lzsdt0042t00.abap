*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0042........................................*
DATA:  BEGIN OF STATUS_ZSDT0042                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0042                      .
CONTROLS: TCTRL_ZSDT0042
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0042                      .
TABLES: ZSDT0042                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
