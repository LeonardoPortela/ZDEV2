*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0135........................................*
DATA:  BEGIN OF STATUS_ZSDT0135                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0135                      .
CONTROLS: TCTRL_ZSDT0135
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0135                      .
TABLES: ZSDT0135                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
