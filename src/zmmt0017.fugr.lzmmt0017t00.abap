*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0017........................................*
DATA:  BEGIN OF STATUS_ZMMT0017                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0017                      .
CONTROLS: TCTRL_ZMMT0017
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0017                      .
TABLES: ZMMT0017                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
