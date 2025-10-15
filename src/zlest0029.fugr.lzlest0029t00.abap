*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0029.......................................*
DATA:  BEGIN OF STATUS_ZLEST0029                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0029                     .
CONTROLS: TCTRL_ZLEST0029
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0029                     .
TABLES: ZLEST0029                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
