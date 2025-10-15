*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT063.........................................*
DATA:  BEGIN OF STATUS_ZGLT063                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT063                       .
CONTROLS: TCTRL_ZGLT063
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT063                       .
TABLES: ZGLT063                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
