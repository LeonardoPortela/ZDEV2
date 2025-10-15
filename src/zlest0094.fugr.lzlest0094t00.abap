*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0094.......................................*
DATA:  BEGIN OF STATUS_ZLEST0094                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0094                     .
CONTROLS: TCTRL_ZLEST0094
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0094                     .
TABLES: ZLEST0094                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
