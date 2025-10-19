*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0014.......................................*
DATA:  BEGIN OF STATUS_ZLEST0014                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0014                     .
CONTROLS: TCTRL_ZLEST0014
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0014                     .
TABLES: ZLEST0014                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
