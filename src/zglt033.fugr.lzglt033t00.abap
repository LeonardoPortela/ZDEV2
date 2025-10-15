*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT033.........................................*
DATA:  BEGIN OF STATUS_ZGLT033                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT033                       .
CONTROLS: TCTRL_ZGLT033
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT033                       .
TABLES: ZGLT033                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
