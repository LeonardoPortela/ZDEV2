*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0040........................................*
DATA:  BEGIN OF STATUS_ZMMT0040                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0040                      .
CONTROLS: TCTRL_ZMMT0040
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0040                      .
TABLES: ZMMT0040                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
