*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0060........................................*
DATA:  BEGIN OF STATUS_ZMMT0060                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0060                      .
CONTROLS: TCTRL_ZMMT0060
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0060                      .
TABLES: ZMMT0060                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
