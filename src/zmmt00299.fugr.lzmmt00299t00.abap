*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0029........................................*
DATA:  BEGIN OF STATUS_ZMMT0029                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0029                      .
CONTROLS: TCTRL_ZMMT0029
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0029                      .
TABLES: ZMMT0029                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
