*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0070........................................*
DATA:  BEGIN OF STATUS_ZMMT0070                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0070                      .
CONTROLS: TCTRL_ZMMT0070
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0070                      .
TABLES: ZMMT0070                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
