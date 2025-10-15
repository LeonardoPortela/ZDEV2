*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIB_NFE_DIST_001................................*
DATA:  BEGIN OF STATUS_ZIB_NFE_DIST_001              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_NFE_DIST_001              .
CONTROLS: TCTRL_ZIB_NFE_DIST_001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZIB_NFE_DIST_IVA................................*
DATA:  BEGIN OF STATUS_ZIB_NFE_DIST_IVA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_NFE_DIST_IVA              .
CONTROLS: TCTRL_ZIB_NFE_DIST_IVA
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZIB_NFE_DIST_001              .
TABLES: *ZIB_NFE_DIST_IVA              .
TABLES: ZIB_NFE_DIST_001               .
TABLES: ZIB_NFE_DIST_IVA               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
