*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0133........................................*
DATA:  BEGIN OF STATUS_ZFIT0133                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0133                      .
CONTROLS: TCTRL_ZFIT0133
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0133                      .
TABLES: ZFIT0133                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
