*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0115........................................*
DATA:  BEGIN OF STATUS_ZSDT0115                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0115                      .
CONTROLS: TCTRL_ZSDT0115
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0115                      .
TABLES: ZSDT0115                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
