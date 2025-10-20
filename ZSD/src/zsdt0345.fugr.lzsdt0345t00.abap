*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0345........................................*
DATA:  BEGIN OF STATUS_ZSDT0345                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0345                      .
CONTROLS: TCTRL_ZSDT0345
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0345                      .
TABLES: ZSDT0345                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
