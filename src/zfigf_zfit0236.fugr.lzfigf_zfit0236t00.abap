*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT0236........................................*
DATA:  BEGIN OF STATUS_ZFIT0236                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT0236                      .
CONTROLS: TCTRL_ZFIT0236
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT0236                      .
TABLES: ZFIT0236                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
