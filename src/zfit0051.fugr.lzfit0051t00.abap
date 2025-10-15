*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0051........................................*
DATA:  BEGIN OF STATUS_ZFIT0051                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0051                      .
CONTROLS: TCTRL_ZFIT0051
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0051                      .
TABLES: ZFIT0051                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
