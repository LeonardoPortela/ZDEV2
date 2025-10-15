*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHCMT_PY_0005...................................*
DATA:  BEGIN OF STATUS_ZHCMT_PY_0005                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHCMT_PY_0005                 .
CONTROLS: TCTRL_ZHCMT_PY_0005
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZHCMT_PY_0005                 .
TABLES: ZHCMT_PY_0005                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
