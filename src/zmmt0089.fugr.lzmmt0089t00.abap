*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0089........................................*
DATA:  BEGIN OF STATUS_ZMMT0089                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0089                      .
CONTROLS: TCTRL_ZMMT0089
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0089                      .
TABLES: ZMMT0089                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
