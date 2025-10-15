*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT084.........................................*
DATA:  BEGIN OF STATUS_ZGLT084                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT084                       .
CONTROLS: TCTRL_ZGLT084
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT084                       .
TABLES: ZGLT084                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
