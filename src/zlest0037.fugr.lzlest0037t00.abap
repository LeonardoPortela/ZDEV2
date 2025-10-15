*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0037.......................................*
DATA:  BEGIN OF STATUS_ZLEST0037                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0037                     .
CONTROLS: TCTRL_ZLEST0037
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0037                     .
TABLES: ZLEST0037                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
