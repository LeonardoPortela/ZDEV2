*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0049........................................*
DATA:  BEGIN OF STATUS_ZFIT0049                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0049                      .
CONTROLS: TCTRL_ZFIT0049
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0049                      .
TABLES: ZFIT0049                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
