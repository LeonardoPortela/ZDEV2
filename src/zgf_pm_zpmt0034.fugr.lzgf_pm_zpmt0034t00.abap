*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0034........................................*
DATA:  BEGIN OF STATUS_ZPMT0034                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0034                      .
CONTROLS: TCTRL_ZPMT0034
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT0034                      .
TABLES: ZPMT0034                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
