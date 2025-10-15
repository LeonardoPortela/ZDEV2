*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT061.........................................*
DATA:  BEGIN OF STATUS_ZGLT061                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT061                       .
CONTROLS: TCTRL_ZGLT061
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT061                       .
TABLES: ZGLT061                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
