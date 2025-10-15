*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDOC_MEMO_FORM_U................................*
DATA:  BEGIN OF STATUS_ZDOC_MEMO_FORM_U              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDOC_MEMO_FORM_U              .
CONTROLS: TCTRL_ZDOC_MEMO_FORM_U
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDOC_MEMO_FORM_U              .
TABLES: ZDOC_MEMO_FORM_U               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
