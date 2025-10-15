*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0044........................................*
DATA:  BEGIN OF STATUS_ZMMT0044                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0044                      .
CONTROLS: TCTRL_ZMMT0044
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0044                      .
TABLES: ZMMT0044                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
