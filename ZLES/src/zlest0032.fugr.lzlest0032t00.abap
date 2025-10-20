*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0032.......................................*
DATA:  BEGIN OF STATUS_ZLEST0032                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0032                     .
CONTROLS: TCTRL_ZLEST0032
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0032                     .
TABLES: ZLEST0032                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
