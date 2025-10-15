*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0025........................................*
DATA:  BEGIN OF STATUS_ZMMT0025                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0025                      .
CONTROLS: TCTRL_ZMMT0025
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0025                      .
TABLES: ZMMT0025                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
