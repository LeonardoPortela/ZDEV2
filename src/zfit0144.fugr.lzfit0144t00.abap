*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0144........................................*
DATA:  BEGIN OF STATUS_ZFIT0144                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0144                      .
CONTROLS: TCTRL_ZFIT0144
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0144                      .
TABLES: ZFIT0144                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
