*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0104.......................................*
DATA:  BEGIN OF STATUS_ZLEST0104                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0104                     .
CONTROLS: TCTRL_ZLEST0104
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0104                     .
TABLES: ZLEST0104                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
