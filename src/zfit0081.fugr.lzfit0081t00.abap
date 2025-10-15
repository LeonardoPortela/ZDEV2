*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0081........................................*
DATA:  BEGIN OF STATUS_ZFIT0081                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0081                      .
CONTROLS: TCTRL_ZFIT0081
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0081                      .
TABLES: ZFIT0081                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
