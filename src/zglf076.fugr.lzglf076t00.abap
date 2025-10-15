*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT076.........................................*
DATA:  BEGIN OF STATUS_ZGLT076                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT076                       .
CONTROLS: TCTRL_ZGLT076
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT076                       .
TABLES: ZGLT076                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
