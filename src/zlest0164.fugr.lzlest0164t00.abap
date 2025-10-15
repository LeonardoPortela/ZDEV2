*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0164.......................................*
DATA:  BEGIN OF STATUS_ZLEST0164                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0164                     .
CONTROLS: TCTRL_ZLEST0164
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0164                     .
TABLES: ZLEST0164                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
