*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0114.......................................*
DATA:  BEGIN OF STATUS_ZLEST0114                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0114                     .
CONTROLS: TCTRL_ZLEST0114
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0114                     .
TABLES: ZLEST0114                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
