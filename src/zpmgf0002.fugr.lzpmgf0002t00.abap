*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0069........................................*
DATA:  BEGIN OF STATUS_ZPMT0069                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0069                      .
CONTROLS: TCTRL_ZPMT0069
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPMT0069                      .
TABLES: ZPMT0069                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
