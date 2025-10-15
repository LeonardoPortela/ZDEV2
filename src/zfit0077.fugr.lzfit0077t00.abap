*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0077........................................*
DATA:  BEGIN OF STATUS_ZFIT0077                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0077                      .
CONTROLS: TCTRL_ZFIT0077
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0077                      .
TABLES: ZFIT0077                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
