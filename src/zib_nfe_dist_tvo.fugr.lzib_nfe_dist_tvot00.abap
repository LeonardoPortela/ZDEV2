*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIB_NFE_DIST_TVO................................*
DATA:  BEGIN OF STATUS_ZIB_NFE_DIST_TVO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_NFE_DIST_TVO              .
CONTROLS: TCTRL_ZIB_NFE_DIST_TVO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIB_NFE_DIST_TVO              .
TABLES: ZIB_NFE_DIST_TVO               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
