*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0053.......................................*
DATA:  BEGIN OF STATUS_ZLEST0053                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0053                     .
CONTROLS: TCTRL_ZLEST0053
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0053                     .
TABLES: ZLEST0053                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
