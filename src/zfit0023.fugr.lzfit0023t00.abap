*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0023........................................*
DATA:  BEGIN OF STATUS_ZFIT0023                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0023                      .
CONTROLS: TCTRL_ZFIT0023
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0023                      .
TABLES: ZFIT0023                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
