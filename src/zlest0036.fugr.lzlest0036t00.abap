*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0036.......................................*
DATA:  BEGIN OF STATUS_ZLEST0036                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0036                     .
CONTROLS: TCTRL_ZLEST0036
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0036                     .
TABLES: ZLEST0036                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
