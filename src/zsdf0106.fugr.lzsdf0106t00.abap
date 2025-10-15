*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0106........................................*
DATA:  BEGIN OF STATUS_ZSDT0106                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0106                      .
CONTROLS: TCTRL_ZSDT0106
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0106                      .
TABLES: ZSDT0106                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
