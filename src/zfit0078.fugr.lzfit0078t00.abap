*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0078........................................*
DATA:  BEGIN OF STATUS_ZFIT0078                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0078                      .
CONTROLS: TCTRL_ZFIT0078
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0078                      .
TABLES: ZFIT0078                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
