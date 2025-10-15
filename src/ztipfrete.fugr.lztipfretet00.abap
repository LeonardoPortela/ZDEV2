*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0159.......................................*
DATA:  BEGIN OF STATUS_ZLEST0159                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0159                     .
CONTROLS: TCTRL_ZLEST0159
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZLEST0160.......................................*
DATA:  BEGIN OF STATUS_ZLEST0160                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0160                     .
CONTROLS: TCTRL_ZLEST0160
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZLEST0159                     .
TABLES: *ZLEST0160                     .
TABLES: ZLEST0159                      .
TABLES: ZLEST0160                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
