*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0092........................................*
DATA:  BEGIN OF STATUS_ZFIT0092                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0092                      .
CONTROLS: TCTRL_ZFIT0092
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0092                      .
TABLES: ZFIT0092                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
