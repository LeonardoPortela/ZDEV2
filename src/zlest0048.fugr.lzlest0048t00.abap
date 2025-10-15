*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0048.......................................*
DATA:  BEGIN OF STATUS_ZLEST0048                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0048                     .
CONTROLS: TCTRL_ZLEST0048
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0048                     .
TABLES: ZLEST0048                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
