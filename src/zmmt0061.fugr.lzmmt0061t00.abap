*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0061........................................*
DATA:  BEGIN OF STATUS_ZMMT0061                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0061                      .
CONTROLS: TCTRL_ZMMT0061
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0061                      .
TABLES: ZMMT0061                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
