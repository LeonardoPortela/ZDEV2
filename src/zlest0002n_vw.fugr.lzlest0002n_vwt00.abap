*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0002N......................................*
DATA:  BEGIN OF STATUS_ZLEST0002N                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0002N                    .
CONTROLS: TCTRL_ZLEST0002N
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0002N                    .
TABLES: ZLEST0002N                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
