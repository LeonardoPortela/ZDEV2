*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0078........................................*
DATA:  BEGIN OF STATUS_ZMMT0078                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0078                      .
CONTROLS: TCTRL_ZMMT0078
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0078                      .
TABLES: ZMMT0078                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
