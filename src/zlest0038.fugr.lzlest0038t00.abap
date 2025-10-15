*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0038.......................................*
DATA:  BEGIN OF STATUS_ZLEST0038                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0038                     .
CONTROLS: TCTRL_ZLEST0038
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0038                     .
TABLES: ZLEST0038                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
