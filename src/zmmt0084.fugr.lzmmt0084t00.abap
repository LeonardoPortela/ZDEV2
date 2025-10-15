*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0084........................................*
DATA:  BEGIN OF STATUS_ZMMT0084                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0084                      .
CONTROLS: TCTRL_ZMMT0084
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0084                      .
TABLES: ZMMT0084                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
