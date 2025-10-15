*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT030.........................................*
DATA:  BEGIN OF STATUS_ZGLT030                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT030                       .
CONTROLS: TCTRL_ZGLT030
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT030                       .
TABLES: ZGLT030                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
