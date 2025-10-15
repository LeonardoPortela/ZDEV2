*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT046.........................................*
DATA:  BEGIN OF STATUS_ZGLT046                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT046                       .
CONTROLS: TCTRL_ZGLT046
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT046                       .
TABLES: ZGLT046                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
