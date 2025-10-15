*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0041........................................*
DATA:  BEGIN OF STATUS_ZMMT0041                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0041                      .
CONTROLS: TCTRL_ZMMT0041
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0041                      .
TABLES: ZMMT0041                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
