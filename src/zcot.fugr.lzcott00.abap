*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCOT0001........................................*
DATA:  BEGIN OF STATUS_ZCOT0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOT0001                      .
CONTROLS: TCTRL_ZCOT0001
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZCOT0002........................................*
DATA:  BEGIN OF STATUS_ZCOT0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOT0002                      .
CONTROLS: TCTRL_ZCOT0002
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZCOT0003........................................*
DATA:  BEGIN OF STATUS_ZCOT0003                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOT0003                      .
CONTROLS: TCTRL_ZCOT0003
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZCOT0005........................................*
DATA:  BEGIN OF STATUS_ZCOT0005                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOT0005                      .
CONTROLS: TCTRL_ZCOT0005
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZCOT0007........................................*
DATA:  BEGIN OF STATUS_ZCOT0007                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOT0007                      .
CONTROLS: TCTRL_ZCOT0007
            TYPE TABLEVIEW USING SCREEN '0005'.
*.........table declarations:.................................*
TABLES: *ZCOT0001                      .
TABLES: *ZCOT0002                      .
TABLES: *ZCOT0003                      .
TABLES: *ZCOT0005                      .
TABLES: *ZCOT0007                      .
TABLES: ZCOT0001                       .
TABLES: ZCOT0002                       .
TABLES: ZCOT0003                       .
TABLES: ZCOT0005                       .
TABLES: ZCOT0007                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
