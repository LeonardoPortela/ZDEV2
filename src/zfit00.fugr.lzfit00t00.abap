*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0008........................................*
DATA:  BEGIN OF STATUS_ZFIT0008                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0008                      .
CONTROLS: TCTRL_ZFIT0008
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFIT0009........................................*
DATA:  BEGIN OF STATUS_ZFIT0009                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0009                      .
CONTROLS: TCTRL_ZFIT0009
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZFIT_APROVADORES................................*
DATA:  BEGIN OF STATUS_ZFIT_APROVADORES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_APROVADORES              .
CONTROLS: TCTRL_ZFIT_APROVADORES
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZFIT0008                      .
TABLES: *ZFIT0009                      .
TABLES: *ZFIT_APROVADORES              .
TABLES: ZFIT0008                       .
TABLES: ZFIT0009                       .
TABLES: ZFIT_APROVADORES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
