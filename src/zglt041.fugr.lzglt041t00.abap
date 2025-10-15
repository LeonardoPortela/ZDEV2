*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT041.........................................*
DATA:  BEGIN OF STATUS_ZGLT041                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT041                       .
CONTROLS: TCTRL_ZGLT041
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT041                       .
TABLES: ZGLT041                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
