*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0156........................................*
DATA:  BEGIN OF STATUS_ZMMT0156                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0156                      .
CONTROLS: TCTRL_ZMMT0156
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZMMT0157........................................*
DATA:  BEGIN OF STATUS_ZMMT0157                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0157                      .
CONTROLS: TCTRL_ZMMT0157
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0156                      .
TABLES: *ZMMT0157                      .
TABLES: ZMMT0156                       .
TABLES: ZMMT0157                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
