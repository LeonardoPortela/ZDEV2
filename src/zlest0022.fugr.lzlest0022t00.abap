*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0022.......................................*
DATA:  BEGIN OF STATUS_ZLEST0022                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0022                     .
CONTROLS: TCTRL_ZLEST0022
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0022                     .
TABLES: ZLEST0022                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
