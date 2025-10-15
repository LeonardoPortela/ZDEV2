*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPPT0004........................................*
DATA:  BEGIN OF STATUS_ZPPT0004                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPPT0004                      .
CONTROLS: TCTRL_ZPPT0004
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZPPT0004                      .
TABLES: ZPPT0004                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
