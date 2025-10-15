*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVARIANTE02.....................................*
DATA:  BEGIN OF STATUS_ZVARIANTE02                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZVARIANTE02                   .
CONTROLS: TCTRL_ZVARIANTE02
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZVARIANTE02                   .
TABLES: ZVARIANTE02                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
