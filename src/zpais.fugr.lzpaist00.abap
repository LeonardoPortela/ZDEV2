*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPAIS...........................................*
DATA:  BEGIN OF STATUS_ZPAIS                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPAIS                         .
CONTROLS: TCTRL_ZPAIS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPAIS                         .
TABLES: ZPAIS                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
