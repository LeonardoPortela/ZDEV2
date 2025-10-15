*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT042.........................................*
DATA:  BEGIN OF STATUS_ZGLT042                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT042                       .
CONTROLS: TCTRL_ZGLT042
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT042                       .
TABLES: ZGLT042                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
