*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0074........................................*
DATA:  BEGIN OF STATUS_ZMMT0074                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0074                      .
CONTROLS: TCTRL_ZMMT0074
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0074                      .
TABLES: ZMMT0074                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
