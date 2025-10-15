*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0034........................................*
DATA:  BEGIN OF STATUS_ZMMT0034                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0034                      .
CONTROLS: TCTRL_ZMMT0034
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0034                      .
TABLES: ZMMT0034                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
