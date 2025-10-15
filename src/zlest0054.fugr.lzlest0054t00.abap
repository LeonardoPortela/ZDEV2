*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0054.......................................*
DATA:  BEGIN OF STATUS_ZLEST0054                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0054                     .
CONTROLS: TCTRL_ZLEST0054
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0054                     .
TABLES: ZLEST0054                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
