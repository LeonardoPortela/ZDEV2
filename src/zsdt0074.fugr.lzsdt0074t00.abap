*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0074........................................*
DATA:  BEGIN OF STATUS_ZSDT0074                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0074                      .
CONTROLS: TCTRL_ZSDT0074
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0074                      .
TABLES: ZSDT0074                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
