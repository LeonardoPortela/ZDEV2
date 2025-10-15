*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT0042........................................*
DATA:  BEGIN OF STATUS_ZPMT0042                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0042                      .
CONTROLS: TCTRL_ZPMT0042
            TYPE TABLEVIEW USING SCREEN '0100'.
*...processing: ZPMT0043........................................*
DATA:  BEGIN OF STATUS_ZPMT0043                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT0043                      .
CONTROLS: TCTRL_ZPMT0043
            TYPE TABLEVIEW USING SCREEN '0200'.
*.........table declarations:.................................*
TABLES: *ZPMT0042                      .
TABLES: *ZPMT0043                      .
TABLES: ZPMT0042                       .
TABLES: ZPMT0043                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
