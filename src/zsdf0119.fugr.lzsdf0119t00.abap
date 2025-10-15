*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0119........................................*
DATA:  BEGIN OF STATUS_ZSDT0119                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0119                      .
CONTROLS: TCTRL_ZSDT0119
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0119                      .
TABLES: ZSDT0119                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
