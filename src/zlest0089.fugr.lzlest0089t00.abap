*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0089.......................................*
DATA:  BEGIN OF STATUS_ZLEST0089                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0089                     .
CONTROLS: TCTRL_ZLEST0089
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0089                     .
TABLES: ZLEST0089                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
