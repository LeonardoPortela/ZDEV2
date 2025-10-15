*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0023........................................*
DATA:  BEGIN OF STATUS_ZMMT0023                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0023                      .
CONTROLS: TCTRL_ZMMT0023
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0023                      .
TABLES: ZMMT0023                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
