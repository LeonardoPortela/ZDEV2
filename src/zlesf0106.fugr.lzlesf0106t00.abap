*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0106.......................................*
DATA:  BEGIN OF STATUS_ZLEST0106                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0106                     .
CONTROLS: TCTRL_ZLEST0106
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0106                     .
TABLES: ZLEST0106                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
