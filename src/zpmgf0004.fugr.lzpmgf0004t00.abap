*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0071........................................*
DATA:  BEGIN OF STATUS_ZPMT0071                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0071                      .
CONTROLS: TCTRL_ZPMT0071
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT0071                      .
TABLES: ZPMT0071                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
