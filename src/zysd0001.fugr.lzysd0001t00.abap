*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZYSD0001........................................*
DATA:  BEGIN OF STATUS_ZYSD0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZYSD0001                      .
CONTROLS: TCTRL_ZYSD0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZYSD0001                      .
TABLES: ZYSD0001                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
