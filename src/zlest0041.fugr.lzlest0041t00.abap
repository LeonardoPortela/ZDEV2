*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0041.......................................*
DATA:  BEGIN OF STATUS_ZLEST0041                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0041                     .
CONTROLS: TCTRL_ZLEST0041
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0041                     .
TABLES: ZLEST0041                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
