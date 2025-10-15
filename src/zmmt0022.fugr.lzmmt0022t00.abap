*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0022........................................*
DATA:  BEGIN OF STATUS_ZMMT0022                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0022                      .
CONTROLS: TCTRL_ZMMT0022
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0022                      .
TABLES: ZMMT0022                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
