*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIB_CTE_DIST_TER................................*
DATA:  BEGIN OF STATUS_ZIB_CTE_DIST_TER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_CTE_DIST_TER              .
CONTROLS: TCTRL_ZIB_CTE_DIST_TER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIB_CTE_DIST_TER              .
TABLES: ZIB_CTE_DIST_TER               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
