*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0180........................................*
DATA:  BEGIN OF STATUS_ZMMT0180                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0180                      .
CONTROLS: TCTRL_ZMMT0180
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0180                      .
TABLES: ZMMT0180                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
