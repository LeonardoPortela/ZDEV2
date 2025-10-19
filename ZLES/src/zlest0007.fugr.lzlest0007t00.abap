*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0007.......................................*
DATA:  BEGIN OF STATUS_ZLEST0007                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0007                     .
CONTROLS: TCTRL_ZLEST0007
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0007                     .
TABLES: ZLEST0007                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
