*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT100.........................................*
DATA:  BEGIN OF STATUS_ZGLT100                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT100                       .
CONTROLS: TCTRL_ZGLT100
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT100                       .
TABLES: ZGLT100                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
