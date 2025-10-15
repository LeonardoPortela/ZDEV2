*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0088........................................*
DATA:  BEGIN OF STATUS_ZFIT0088                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0088                      .
CONTROLS: TCTRL_ZFIT0088
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0088                      .
TABLES: ZFIT0088                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
