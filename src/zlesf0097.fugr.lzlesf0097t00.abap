*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0097.......................................*
DATA:  BEGIN OF STATUS_ZLEST0097                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0097                     .
CONTROLS: TCTRL_ZLEST0097
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0097                     .
TABLES: ZLEST0097                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
