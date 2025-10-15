*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0017.......................................*
DATA:  BEGIN OF STATUS_ZLEST0017                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0017                     .
CONTROLS: TCTRL_ZLEST0017
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0017                     .
TABLES: ZLEST0017                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
