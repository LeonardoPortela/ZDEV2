*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPPT0001........................................*
DATA:  BEGIN OF STATUS_ZPPT0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPPT0001                      .
CONTROLS: TCTRL_ZPPT0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPPT0001                      .
TABLES: ZPPT0001                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
