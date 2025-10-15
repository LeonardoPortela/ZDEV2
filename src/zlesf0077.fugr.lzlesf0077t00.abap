*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0077.......................................*
DATA:  BEGIN OF STATUS_ZLEST0077                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0077                     .
CONTROLS: TCTRL_ZLEST0077
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0077                     .
TABLES: ZLEST0077                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
