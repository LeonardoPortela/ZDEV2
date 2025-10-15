*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0031........................................*
DATA:  BEGIN OF STATUS_ZMMT0031                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0031                      .
CONTROLS: TCTRL_ZMMT0031
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0031                      .
TABLES: ZMMT0031                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
