*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0022........................................*
DATA:  BEGIN OF STATUS_ZSDT0022                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0022                      .
CONTROLS: TCTRL_ZSDT0022
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0022                      .
TABLES: ZSDT0022                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
