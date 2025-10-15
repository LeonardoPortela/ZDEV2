*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0011........................................*
DATA:  BEGIN OF STATUS_ZSDT0011                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0011                      .
CONTROLS: TCTRL_ZSDT0011
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0011                      .
TABLES: ZSDT0011                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
