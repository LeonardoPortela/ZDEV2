*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCMT0008.......................................*
DATA:  BEGIN OF STATUS_ZHCMT0008                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHCMT0008                     .
CONTROLS: TCTRL_ZHCMT0008
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHCMT0008                     .
TABLES: ZHCMT0008                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
