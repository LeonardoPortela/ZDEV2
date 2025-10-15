*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0099........................................*
DATA:  BEGIN OF STATUS_ZSDT0099                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0099                      .
CONTROLS: TCTRL_ZSDT0099
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0099                      .
TABLES: ZSDT0099                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
