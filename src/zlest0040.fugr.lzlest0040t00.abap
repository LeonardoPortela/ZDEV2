*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0040.......................................*
DATA:  BEGIN OF STATUS_ZLEST0040                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0040                     .
CONTROLS: TCTRL_ZLEST0040
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0040                     .
TABLES: ZLEST0040                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
