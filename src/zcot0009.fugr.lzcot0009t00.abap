*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCOT0009........................................*
DATA:  BEGIN OF STATUS_ZCOT0009                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOT0009                      .
CONTROLS: TCTRL_ZCOT0009
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZCOT0009                      .
TABLES: ZCOT0009                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
