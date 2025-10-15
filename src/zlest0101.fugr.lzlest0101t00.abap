*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0101.......................................*
DATA:  BEGIN OF STATUS_ZLEST0101                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0101                     .
CONTROLS: TCTRL_ZLEST0101
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0101                     .
TABLES: ZLEST0101                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
