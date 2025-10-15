*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0132.......................................*
DATA:  BEGIN OF STATUS_ZLEST0132                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0132                     .
CONTROLS: TCTRL_ZLEST0132
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0132                     .
TABLES: ZLEST0132                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
