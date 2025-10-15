*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0105........................................*
DATA:  BEGIN OF STATUS_ZFIT0105                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0105                      .
CONTROLS: TCTRL_ZFIT0105
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0105                      .
TABLES: ZFIT0105                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
