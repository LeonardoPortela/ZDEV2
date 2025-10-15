*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0009........................................*
DATA:  BEGIN OF STATUS_ZSDT0009                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0009                      .
CONTROLS: TCTRL_ZSDT0009
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0009                      .
TABLES: ZSDT0009                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
