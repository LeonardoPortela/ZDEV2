*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0133.......................................*
DATA:  BEGIN OF STATUS_ZLEST0133                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0133                     .
CONTROLS: TCTRL_ZLEST0133
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0133                     .
TABLES: ZLEST0133                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
