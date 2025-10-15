*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVARIANTE01.....................................*
DATA:  BEGIN OF STATUS_ZVARIANTE01                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZVARIANTE01                   .
CONTROLS: TCTRL_ZVARIANTE01
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZVARIANTE01                   .
TABLES: ZVARIANTE01                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
