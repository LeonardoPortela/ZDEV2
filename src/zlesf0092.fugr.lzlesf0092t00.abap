*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0092.......................................*
DATA:  BEGIN OF STATUS_ZLEST0092                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0092                     .
CONTROLS: TCTRL_ZLEST0092
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0092                     .
TABLES: ZLEST0092                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
