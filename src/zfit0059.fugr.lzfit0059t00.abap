*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0059........................................*
DATA:  BEGIN OF STATUS_ZFIT0059                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0059                      .
CONTROLS: TCTRL_ZFIT0059
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0059                      .
TABLES: ZFIT0059                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
