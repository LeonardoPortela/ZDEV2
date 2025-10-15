*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0005........................................*
DATA:  BEGIN OF STATUS_ZMMT0005                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0005                      .
CONTROLS: TCTRL_ZMMT0005
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0005                      .
TABLES: ZMMT0005                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
