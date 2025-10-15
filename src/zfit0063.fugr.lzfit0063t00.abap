*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0063........................................*
DATA:  BEGIN OF STATUS_ZFIT0063                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0063                      .
CONTROLS: TCTRL_ZFIT0063
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0063                      .
TABLES: ZFIT0063                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
