*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0047........................................*
DATA:  BEGIN OF STATUS_ZSDT0047                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0047                      .
CONTROLS: TCTRL_ZSDT0047
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0047                      .
TABLES: ZSDT0047                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
