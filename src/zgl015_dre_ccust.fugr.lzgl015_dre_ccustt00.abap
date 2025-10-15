*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL015_DRE_CCUST................................*
DATA:  BEGIN OF STATUS_ZGL015_DRE_CCUST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL015_DRE_CCUST              .
CONTROLS: TCTRL_ZGL015_DRE_CCUST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL015_DRE_CCUST              .
TABLES: ZGL015_DRE_CCUST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
