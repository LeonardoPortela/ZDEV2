*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0046........................................*
DATA:  BEGIN OF STATUS_ZMMT0046                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0046                      .
CONTROLS: TCTRL_ZMMT0046
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0046                      .
TABLES: ZMMT0046                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
