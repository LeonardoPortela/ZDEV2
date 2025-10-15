*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0064.......................................*
DATA:  BEGIN OF STATUS_ZLEST0064                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0064                     .
CONTROLS: TCTRL_ZLEST0064
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0064                     .
TABLES: ZLEST0064                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
