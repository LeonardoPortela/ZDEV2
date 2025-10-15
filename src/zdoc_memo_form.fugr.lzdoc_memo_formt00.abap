*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDOC_MEMO_FORM..................................*
DATA:  BEGIN OF STATUS_ZDOC_MEMO_FORM                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDOC_MEMO_FORM                .
CONTROLS: TCTRL_ZDOC_MEMO_FORM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDOC_MEMO_FORM                .
TABLES: ZDOC_MEMO_FORM                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
