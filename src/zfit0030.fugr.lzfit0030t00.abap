*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0030........................................*
DATA:  BEGIN OF STATUS_ZFIT0030                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0030                      .
CONTROLS: TCTRL_ZFIT0030
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0030                      .
TABLES: ZFIT0030                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
