*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0120........................................*
DATA:  BEGIN OF STATUS_ZSDT0120                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0120                      .
CONTROLS: TCTRL_ZSDT0120
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0120                      .
TABLES: ZSDT0120                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
