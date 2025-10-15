*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0154.......................................*
DATA:  BEGIN OF STATUS_ZLEST0154                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0154                     .
CONTROLS: TCTRL_ZLEST0154
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0154                     .
TABLES: ZLEST0154                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
