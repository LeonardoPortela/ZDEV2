*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT053.........................................*
DATA:  BEGIN OF STATUS_ZGLT053                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT053                       .
CONTROLS: TCTRL_ZGLT053
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT053                       .
TABLES: ZGLT053                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
