*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0093.......................................*
DATA:  BEGIN OF STATUS_ZLEST0093                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0093                     .
CONTROLS: TCTRL_ZLEST0093
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0093                     .
TABLES: ZLEST0093                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
