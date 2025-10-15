*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0039........................................*
DATA:  BEGIN OF STATUS_ZFIT0039                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0039                      .
CONTROLS: TCTRL_ZFIT0039
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0039                      .
TABLES: ZFIT0039                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
