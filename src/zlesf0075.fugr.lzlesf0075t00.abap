*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0075.......................................*
DATA:  BEGIN OF STATUS_ZLEST0075                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0075                     .
CONTROLS: TCTRL_ZLEST0075
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0075                     .
TABLES: ZLEST0075                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
