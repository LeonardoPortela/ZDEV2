*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0070........................................*
DATA:  BEGIN OF STATUS_ZPMT0070                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0070                      .
CONTROLS: TCTRL_ZPMT0070
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT0070                      .
TABLES: ZPMT0070                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
