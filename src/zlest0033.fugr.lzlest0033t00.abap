*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0033.......................................*
DATA:  BEGIN OF STATUS_ZLEST0033                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0033                     .
CONTROLS: TCTRL_ZLEST0033
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0033                     .
TABLES: ZLEST0033                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
