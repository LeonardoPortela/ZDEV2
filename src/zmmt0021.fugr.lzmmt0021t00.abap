*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0021........................................*
DATA:  BEGIN OF STATUS_ZMMT0021                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0021                      .
CONTROLS: TCTRL_ZMMT0021
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0021                      .
TABLES: ZMMT0021                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
