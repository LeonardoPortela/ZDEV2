*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0094........................................*
DATA:  BEGIN OF STATUS_ZMMT0094                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0094                      .
CONTROLS: TCTRL_ZMMT0094
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0094                      .
TABLES: ZMMT0094                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
