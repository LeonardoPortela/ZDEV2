*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0335........................................*
DATA:  BEGIN OF STATUS_ZSDT0335                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0335                      .
CONTROLS: TCTRL_ZSDT0335
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0335                      .
TABLES: ZSDT0335                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
