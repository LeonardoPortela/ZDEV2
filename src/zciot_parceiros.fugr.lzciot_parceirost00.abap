*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCIOT_PARCEIROS.................................*
DATA:  BEGIN OF STATUS_ZCIOT_PARCEIROS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCIOT_PARCEIROS               .
CONTROLS: TCTRL_ZCIOT_PARCEIROS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCIOT_PARCEIROS               .
TABLES: ZCIOT_PARCEIROS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
