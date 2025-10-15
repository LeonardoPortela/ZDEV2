*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0007........................................*
DATA:  BEGIN OF STATUS_ZMMT0007                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0007                      .
CONTROLS: TCTRL_ZMMT0007
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0007                      .
TABLES: ZMMT0007                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
