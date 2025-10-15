*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT065.........................................*
DATA:  BEGIN OF STATUS_ZGLT065                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT065                       .
CONTROLS: TCTRL_ZGLT065
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT065                       .
TABLES: ZGLT065                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
