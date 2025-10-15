*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0018.......................................*
DATA:  BEGIN OF STATUS_ZLEST0018                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0018                     .
CONTROLS: TCTRL_ZLEST0018
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0018                     .
TABLES: ZLEST0018                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
