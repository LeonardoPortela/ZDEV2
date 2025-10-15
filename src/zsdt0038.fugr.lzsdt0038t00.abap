*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0038........................................*
DATA:  BEGIN OF STATUS_ZSDT0038                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0038                      .
CONTROLS: TCTRL_ZSDT0038
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0038                      .
TABLES: ZSDT0038                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
