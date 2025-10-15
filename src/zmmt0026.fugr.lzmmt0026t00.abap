*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0026........................................*
DATA:  BEGIN OF STATUS_ZMMT0026                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0026                      .
CONTROLS: TCTRL_ZMMT0026
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0026                      .
TABLES: ZMMT0026                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
