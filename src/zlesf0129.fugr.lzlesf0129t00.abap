*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0129.......................................*
DATA:  BEGIN OF STATUS_ZLEST0129                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0129                     .
CONTROLS: TCTRL_ZLEST0129
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0129                     .
TABLES: ZLEST0129                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
