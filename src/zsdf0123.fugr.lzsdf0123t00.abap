*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0123........................................*
DATA:  BEGIN OF STATUS_ZSDT0123                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0123                      .
CONTROLS: TCTRL_ZSDT0123
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0123                      .
TABLES: ZSDT0123                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
