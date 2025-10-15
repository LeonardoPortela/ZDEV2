*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0047.......................................*
DATA:  BEGIN OF STATUS_ZLEST0047                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0047                     .
CONTROLS: TCTRL_ZLEST0047
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0047                     .
TABLES: ZLEST0047                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
