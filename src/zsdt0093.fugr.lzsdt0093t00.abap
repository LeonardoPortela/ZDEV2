*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0093........................................*
DATA:  BEGIN OF STATUS_ZSDT0093                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0093                      .
CONTROLS: TCTRL_ZSDT0093
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0093                      .
TABLES: ZSDT0093                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
