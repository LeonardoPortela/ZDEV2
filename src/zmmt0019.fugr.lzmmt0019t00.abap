*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0019........................................*
DATA:  BEGIN OF STATUS_ZMMT0019                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0019                      .
CONTROLS: TCTRL_ZMMT0019
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0019                      .
TABLES: ZMMT0019                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
