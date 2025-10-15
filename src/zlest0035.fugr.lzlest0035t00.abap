*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0035.......................................*
DATA:  BEGIN OF STATUS_ZLEST0035                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0035                     .
CONTROLS: TCTRL_ZLEST0035
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0035                     .
TABLES: ZLEST0035                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
