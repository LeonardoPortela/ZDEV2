*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0058........................................*
DATA:  BEGIN OF STATUS_ZFIT0058                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0058                      .
CONTROLS: TCTRL_ZFIT0058
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0058                      .
TABLES: ZFIT0058                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
