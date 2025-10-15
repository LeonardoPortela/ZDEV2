*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0101........................................*
DATA:  BEGIN OF STATUS_ZSDT0101                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0101                      .
CONTROLS: TCTRL_ZSDT0101
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0101                      .
TABLES: ZSDT0101                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
