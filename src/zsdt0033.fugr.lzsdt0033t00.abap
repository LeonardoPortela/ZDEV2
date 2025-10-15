*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0033........................................*
DATA:  BEGIN OF STATUS_ZSDT0033                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0033                      .
CONTROLS: TCTRL_ZSDT0033
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0033                      .
TABLES: ZSDT0033                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
