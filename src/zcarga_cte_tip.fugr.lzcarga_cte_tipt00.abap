*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCARGA_CTE_TIP..................................*
DATA:  BEGIN OF STATUS_ZCARGA_CTE_TIP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCARGA_CTE_TIP                .
CONTROLS: TCTRL_ZCARGA_CTE_TIP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCARGA_CTE_TIP                .
TABLES: ZCARGA_CTE_TIP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
