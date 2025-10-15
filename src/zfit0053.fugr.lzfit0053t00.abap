*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0053........................................*
DATA:  BEGIN OF STATUS_ZFIT0053                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0053                      .
CONTROLS: TCTRL_ZFIT0053
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFIT0053                      .
TABLES: ZFIT0053                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
