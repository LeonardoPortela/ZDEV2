*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0117.......................................*
DATA:  BEGIN OF STATUS_ZLEST0117                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0117                     .
CONTROLS: TCTRL_ZLEST0117
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0117                     .
TABLES: ZLEST0117                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
