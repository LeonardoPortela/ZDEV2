*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0166........................................*
DATA:  BEGIN OF STATUS_ZMMT0166                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0166                      .
CONTROLS: TCTRL_ZMMT0166
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0166                      .
TABLES: ZMMT0166                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
