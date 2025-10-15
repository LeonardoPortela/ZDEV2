*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0136.......................................*
DATA:  BEGIN OF STATUS_ZLEST0136                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0136                     .
CONTROLS: TCTRL_ZLEST0136
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0136                     .
TABLES: ZLEST0136                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
