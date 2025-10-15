*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT092A........................................*
DATA:  BEGIN OF STATUS_ZGLT092A                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT092A                      .
CONTROLS: TCTRL_ZGLT092A
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT092A                      .
TABLES: ZGLT092A                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
