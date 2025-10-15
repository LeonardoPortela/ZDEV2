*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDOC_MEMO_NR....................................*
DATA:  BEGIN OF STATUS_ZDOC_MEMO_NR                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDOC_MEMO_NR                  .
CONTROLS: TCTRL_ZDOC_MEMO_NR
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZDOC_MEMO_NR                  .
TABLES: ZDOC_MEMO_NR                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
