*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCOT0010........................................*
DATA:  BEGIN OF STATUS_ZCOT0010                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOT0010                      .
CONTROLS: TCTRL_ZCOT0010
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZCOT0010                      .
TABLES: ZCOT0010                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
