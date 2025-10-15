*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0048........................................*
DATA:  BEGIN OF STATUS_ZMMT0048                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0048                      .
CONTROLS: TCTRL_ZMMT0048
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0048                      .
TABLES: ZMMT0048                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
