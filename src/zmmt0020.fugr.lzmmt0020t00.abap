*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0020........................................*
DATA:  BEGIN OF STATUS_ZMMT0020                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0020                      .
CONTROLS: TCTRL_ZMMT0020
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0020                      .
TABLES: ZMMT0020                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
