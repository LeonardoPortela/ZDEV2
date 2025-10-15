*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT060.........................................*
DATA:  BEGIN OF STATUS_ZGLT060                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT060                       .
CONTROLS: TCTRL_ZGLT060
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT060                       .
TABLES: ZGLT060                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
