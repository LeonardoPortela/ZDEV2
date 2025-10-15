*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0077........................................*
DATA:  BEGIN OF STATUS_ZSDT0077                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0077                      .
CONTROLS: TCTRL_ZSDT0077
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0077                      .
TABLES: ZSDT0077                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
