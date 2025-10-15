*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT057.........................................*
DATA:  BEGIN OF STATUS_ZGLT057                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT057                       .
CONTROLS: TCTRL_ZGLT057
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT057                       .
TABLES: ZGLT057                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
