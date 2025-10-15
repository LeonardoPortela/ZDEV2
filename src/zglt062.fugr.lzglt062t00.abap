*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT062.........................................*
DATA:  BEGIN OF STATUS_ZGLT062                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT062                       .
CONTROLS: TCTRL_ZGLT062
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT062                       .
TABLES: ZGLT062                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
