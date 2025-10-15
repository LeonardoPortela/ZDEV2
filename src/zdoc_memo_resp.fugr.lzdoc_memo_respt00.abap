*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDOC_MEMO_RESP..................................*
DATA:  BEGIN OF STATUS_ZDOC_MEMO_RESP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDOC_MEMO_RESP                .
CONTROLS: TCTRL_ZDOC_MEMO_RESP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDOC_MEMO_RESP                .
TABLES: ZDOC_MEMO_RESP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
