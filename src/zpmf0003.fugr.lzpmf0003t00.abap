*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0021........................................*
DATA:  BEGIN OF STATUS_ZPMT0021                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0021                      .
CONTROLS: TCTRL_ZPMT0021
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZPMT0021                      .
TABLES: ZPMT0021                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
