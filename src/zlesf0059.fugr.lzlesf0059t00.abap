*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0059.......................................*
DATA:  BEGIN OF STATUS_ZLEST0059                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0059                     .
CONTROLS: TCTRL_ZLEST0059
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0059                     .
TABLES: ZLEST0059                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
