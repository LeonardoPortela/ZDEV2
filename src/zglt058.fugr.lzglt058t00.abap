*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT058.........................................*
DATA:  BEGIN OF STATUS_ZGLT058                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT058                       .
CONTROLS: TCTRL_ZGLT058
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT058                       .
TABLES: ZGLT058                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
